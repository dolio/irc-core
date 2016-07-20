{-# LANGUAGE OverloadedStrings #-}

module Client.Commands
  ( CommandResult(..)
  , executeCommand
  , nickTabCompletion
  ) where

import           Client.Configuration
import           Client.ConnectionState
import           Client.Message
import           Client.NetworkConnection
import           Client.ServerSettings
import           Client.ChannelState
import           Client.State
import           Client.Window
import           Client.WordCompletion
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import           Irc.UserInfo
import           Irc.Modes
import qualified Client.EditBox as Edit

data CommandResult
  = CommandContinue ClientState
  | CommandQuit

type ClientCommand = ClientState -> String -> IO CommandResult
type NetworkCommand = NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
type ChannelCommand = NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult

-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a bool
-- indicating that tab completion should be reversed
data Command
  = ClientCommand  ClientCommand  (Bool -> ClientCommand)
  | NetworkCommand NetworkCommand (Bool -> NetworkCommand)
  | ChannelCommand ChannelCommand (Bool -> ChannelCommand)

addConnection :: String -> ClientState -> IO ClientState
addConnection host st =
  do let network = Text.pack host
         defSettings = (view (clientConfig . configDefaults) st)
                     { _ssHostName = host }
         settings = fromMaybe defSettings
                              (view (clientConfig . configServers . at host) st)

     c <- createConnection
            network
            (view clientConnectionContext st)
            settings
            (view clientEvents st)

     let cs = newConnectionState settings c
     traverse_ (sendMsg cs) (initialMessages cs)

     return $! set (clientConnections . at network) (Just cs) st


commandContinue :: Monad m => ClientState -> m CommandResult
commandContinue = return . CommandContinue

splitWord :: String -> (String, String)
splitWord str = (w, drop 1 rest)
  where
    (w, rest) = break isSpace str

nextWord :: String -> (String, String)
nextWord = splitWord . dropWhile isSpace

-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand :: Maybe Bool -> String -> ClientState -> IO CommandResult

executeCommand (Just isReversed) _ st
  | Just st' <- commandNameCompletion isReversed st = commandContinue st'

executeCommand tabCompleteReversed str st =
  let (cmd, rest) = splitWord str
      cmdTxt      = Text.toLower (Text.pack cmd) in
  case HashMap.lookup cmdTxt commands of

    Just (ClientCommand exec tab) ->
          maybe exec tab tabCompleteReversed
            st rest

    Just (NetworkCommand exec tab)
      | Just network <- views clientFocus focusNetwork st
      , Just cs      <- preview (clientConnections . ix network) st ->
          maybe exec tab tabCompleteReversed
            network cs st rest

    Just (ChannelCommand exec tab)
      | ChannelFocus network channelId <- view clientFocus st
      , Just cs <- preview (clientConnections . ix network) st
      , isChannelIdentifier cs channelId ->
          maybe exec tab tabCompleteReversed
            network cs channelId st rest

    _ -> commandContinue st

commands :: HashMap Text Command
commands = HashMap.fromList
  [ ("connect", ClientCommand cmdConnect noClientTab)
  , ("exit"   , ClientCommand cmdExit    noClientTab)
  , ("focus"  , ClientCommand cmdFocus   simpleClientTab)
  , ("clear"  , ClientCommand cmdClear   noClientTab)

  , ("quote"  , NetworkCommand cmdQuote  simpleNetworkTab)
  , ("join"   , NetworkCommand cmdJoin   simpleNetworkTab)
  , ("mode"   , NetworkCommand cmdMode   simpleNetworkTab)
  , ("msg"    , NetworkCommand cmdMsg    simpleNetworkTab)
  , ("nick"   , NetworkCommand cmdNick   simpleNetworkTab)
  , ("quit"   , NetworkCommand cmdQuit   simpleNetworkTab)
  , ("whois"  , NetworkCommand cmdWhois  simpleNetworkTab)
  , ("whowas" , NetworkCommand cmdWhowas simpleNetworkTab)

  , ("invite" , ChannelCommand cmdInvite simpleChannelTab)
  , ("topic"  , ChannelCommand cmdTopic  tabTopic    )
  , ("kick"   , ChannelCommand cmdKick   simpleChannelTab)
  , ("remove" , ChannelCommand cmdRemove simpleChannelTab)
  , ("me"     , ChannelCommand cmdMe     simpleChannelTab)
  , ("part"   , ChannelCommand cmdPart   simpleChannelTab)

  , ("users"  , ChannelCommand cmdUsers  noChannelTab)
  , ("masks"  , ChannelCommand cmdMasks  noChannelTab)
  ]

noClientTab :: Bool -> ClientCommand
noClientTab _ st _ = commandContinue st

noChannelTab :: Bool -> ChannelCommand
noChannelTab _ _ _ _ st _ = commandContinue st

simpleClientTab :: Bool -> ClientCommand
simpleClientTab isReversed st _ =
  commandContinue (nickTabCompletion isReversed st)

simpleNetworkTab :: Bool -> NetworkCommand
simpleNetworkTab isReversed _ _ st _ =
  commandContinue (nickTabCompletion isReversed st)

simpleChannelTab :: Bool -> ChannelCommand
simpleChannelTab isReversed _ _ _ st _ =
  commandContinue (nickTabCompletion isReversed st)

cmdExit :: ClientState -> String -> IO CommandResult
cmdExit _ _ = return CommandQuit

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: ClientState -> String -> IO CommandResult
cmdClear st _
  = commandContinue
  $ windowEffect
  $ consumeInput st
  where
    windowEffect
      | isActive  = clearWindow
      | otherwise = deleteWindow

    deleteWindow = advanceFocus . setWindow Nothing
    clearWindow  =                setWindow (Just emptyWindow)

    setWindow = set (clientWindows . at (view clientFocus st))

    isActive =
      case view clientFocus st of
        Unfocused -> False
        NetworkFocus network ->
            has (clientConnections . ix network) st
        ChannelFocus network channel ->
            has ( clientConnections . ix network
                . csChannels        . ix channel) st


cmdQuote :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdQuote _ cs st rest =
  case parseRawIrcMsg (Text.pack rest) of
    Nothing  -> commandContinue st
    Just raw ->
      do sendMsg cs raw
         commandContinue (consumeInput st)

-- | Implementation of @/me@
cmdMe :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdMe network cs channelId st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         myNick = UserInfo (view csNick cs) Nothing Nothing
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Action myNick channelId (Text.pack rest))
                    }
     sendMsg cs (rawIrcMsg "PRIVMSG" [idText channelId, actionTxt])
     commandContinue
       $ recordChannelMessage network channelId entry
       $ consumeInput st

-- | Implementation of @/msg@
cmdMsg :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdMsg network cs st rest =
  do now <- getZonedTime
     let (targetsStr, msgStr) = nextWord rest
         targetTxts = Text.split (==',') (Text.pack targetsStr)
         targetIds  = mkId <$> targetTxts
         msgTxt = Text.pack msgStr
         myNick = UserInfo (view csNick cs) Nothing Nothing
         entries = [ (targetId,
                      ClientMessage
                      { _msgTime = now
                      , _msgNetwork = network
                      , _msgBody = IrcBody (Privmsg myNick targetId msgTxt)
                      })
                   | targetId <- targetIds ]

     for_ targetTxts $ \targetTxt ->
       sendMsg cs (rawIrcMsg "PRIVMSG" [targetTxt, msgTxt])

     let st' = foldl' (\acc (targetId, entry) ->
                         recordChannelMessage network targetId entry acc)
                      st
                      entries

     commandContinue (consumeInput st')

cmdConnect :: ClientState -> String -> IO CommandResult
cmdConnect st rest =
  case words rest of
    [network] ->
      do st' <- addConnection network $ consumeInput st
         commandContinue $ changeFocus (NetworkFocus (Text.pack network)) st'
    _ -> commandContinue st

cmdFocus :: ClientState -> String -> IO CommandResult
cmdFocus st rest =
  case words rest of
    [network] ->
      let focus = NetworkFocus (Text.pack network) in
      commandContinue
        $ changeFocus focus
        $ consumeInput st

    [network,channel] ->
      let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
      commandContinue
        $ changeFocus focus
        $ consumeInput st

    _ -> commandContinue st

cmdWhois :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhois _ cs st rest =
  do sendMsg cs (rawIrcMsg "WHOIS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdWhowas :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhowas _ cs st rest =
  do sendMsg cs (rawIrcMsg "WHOWAS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdMode :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdMode _ cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdNick _ cs st rest =
  case words rest of
    [nick] ->
      do sendMsg cs (rawIrcMsg "NICK" [Text.pack nick])
         commandContinue (consumeInput st)
    _ -> commandContinue st

cmdPart :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdPart _ cs channelId st rest =
  do let msgs = case dropWhile isSpace rest of
                  ""  -> []
                  msg -> [Text.pack msg]
     sendMsg cs (rawIrcMsg "PART" (idText channelId : msgs))
     commandContinue (consumeInput st)

cmdInvite :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdInvite _ cs channelId st rest =
  case words rest of
    [nick] ->
      do sendMsg cs (rawIrcMsg "INVITE" [Text.pack nick, idText channelId])
         commandContinue (consumeInput st)
    _ -> commandContinue st

cmdTopic :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdTopic _ cs channelId st rest =
  do let topics = case dropWhile isSpace rest of
                    ""    -> []
                    topic -> [Text.pack topic]
     sendMsg cs (rawIrcMsg "TOPIC" (idText channelId : topics))
     commandContinue (consumeInput st)

tabTopic :: Bool -> NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
tabTopic _ _ cs channelId st rest

  | all isSpace rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = Edit.end
                    . set Edit.content ("/topic " ++ Text.unpack topic)
        commandContinue (over clientTextBox textBox st)

  | otherwise = commandContinue st

cmdUsers :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdUsers _ _ _ st _ = commandContinue
                    $ changeSubfocus FocusUsers
                    $ consumeInput st

cmdMasks :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdMasks _ cs _ st rest =
  case words rest of
    [[mode]] | mode `elem` view (csModeTypes . modesLists) cs ->
        commandContinue $ changeSubfocus (FocusMasks mode)
                        $ consumeInput st
    _ -> commandContinue st

cmdKick :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdKick _ cs channelId st rest =
  do let (who,reason) = nextWord rest
         msgs = case dropWhile isSpace reason of
                  "" -> []
                  msg -> [Text.pack msg]
     sendMsg cs $ rawIrcMsg "KICK" (idText channelId : Text.pack who : msgs)
     commandContinue $ consumeInput st

cmdRemove :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdRemove _ cs channelId st rest =
  do let (who,reason) = nextWord rest
         msgs = case dropWhile isSpace reason of
                  "" -> []
                  msg -> [Text.pack msg]
     sendMsg cs $ rawIrcMsg "REMOVE" (idText channelId : Text.pack who : msgs)
     commandContinue $ consumeInput st

cmdJoin :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdJoin network cs st rest =
  let ws = words rest
      doJoin channelTxt =
        do let channelId = mkId (Text.pack (takeWhile (/=',') channelTxt))
           sendMsg cs $ rawIrcMsg "JOIN" (Text.pack <$> ws)
           commandContinue
               $ changeFocus (ChannelFocus network channelId)
               $ consumeInput st
  in case ws of
       [channelTxt]   -> doJoin channelTxt
       [channelTxt,_] -> doJoin channelTxt
       _ -> commandContinue st


cmdQuit :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdQuit _ cs st rest =
  do let msgs = case dropWhile isSpace rest of
                  ""  -> []
                  msg -> [Text.pack msg]
     sendMsg cs (rawIrcMsg "QUIT" msgs)
     commandContinue (consumeInput st)

modeCommand :: [Text] -> ConnectionState -> ClientState -> IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (rawIrcMsg "MODE" (idText (view csNick cs) : modes))
         commandContinue (consumeInput st)

    ChannelFocus _ chan ->
          do sendMsg cs (rawIrcMsg "MODE" (idText chan : modes))
             commandContinue (consumeInput st)

    _ -> commandContinue st

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete id isReversed possibilities) st
  where
    n = length leadingPart
    leadingPart = takeWhile (not . isSpace) (clientInput st)
    cursorPos   = view (clientTextBox . Edit.pos) st
    possibilities = mkId . Text.cons '/' <$> HashMap.keys commands

nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> ClientState
nickTabCompletion isReversed st
  = fromMaybe st
  $ clientTextBox (wordComplete (++": ") isReversed completions) st
  where
    completions = currentUserList st
