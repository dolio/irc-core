{-# LANGUAGE BangPatterns, OverloadedStrings, ExistentialQuantification #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands
  ( CommandResult(..)
  , execute
  , executeUserCommand
  , commandExpansion
  , tabCompletion
  -- * Commands
  , Command(..)
  , CommandImpl(..)
  , commands
  , commandsList
  ) where

import           Client.CApi
import           Client.Commands.Arguments
import           Client.Commands.Exec
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Client.Message
import           Client.State
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Applicative
import           Control.Exception (displayException, try)
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.HashSet (HashSet)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.List.Split
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Time
import           Irc.Commands
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import           Irc.UserInfo
import           Irc.Modes
import           LensUtils
import           System.Process
import           Text.Read
import           Text.Regex.TDFA

-- | Possible results of running a command
data CommandResult
  -- | Continue running the client, consume input if command was from input
  = CommandSuccess ClientState
  -- | Continue running the client, report an error
  | CommandFailure ClientState
  -- | Client should close
  | CommandQuit ClientState


-- | Type of commands that always work
type ClientCommand a = ClientState -> a {- ^ arguments -} -> IO CommandResult

-- | Type of commands that require an active network to be focused
type NetworkCommand a = NetworkState {- ^ current network -} -> ClientCommand a

-- | Type of commands that require an active channel to be focused
type ChannelCommand a = Identifier {- ^ focused channel -} -> NetworkCommand a


-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a bool
-- indicating that tab completion should be reversed
data CommandImpl a
  -- | no requirements
  = ClientCommand  (ClientCommand  a) (Bool -> ClientCommand  String)
  -- | requires an active network
  | NetworkCommand (NetworkCommand a) (Bool -> NetworkCommand String)
  -- | requires an active chat window
  | ChatCommand    (ChannelCommand a) (Bool -> ChannelCommand String)
  -- | requires an active channel window
  | ChannelCommand (ChannelCommand a) (Bool -> ChannelCommand String)

-- | A command is an argument specification, implementation, and documentation
data Command = forall a. Command (ArgumentSpec a) Text (CommandImpl a)

-- | Consider the text entry successful and resume the client
commandSuccess :: Monad m => ClientState -> m CommandResult
commandSuccess = return . CommandSuccess

-- | Consider the text entry a failure and resume the client
commandFailure :: Monad m => ClientState -> m CommandResult
commandFailure = return . CommandFailure

-- | Command failure with an error message printed to client window
commandFailureMsg :: Text -> ClientState -> IO CommandResult
commandFailureMsg e st =
  do now <- getZonedTime
     return $! CommandFailure $! recordError now st e

-- | Interpret the given chat message or command. Leading @/@ indicates a
-- command. Otherwise if a channel or user query is focused a chat message
-- will be sent.
execute ::
  String {- ^ chat or command -} ->
  ClientState -> IO CommandResult
execute str st =
  case str of
    []          -> commandFailure st
    '/':command -> executeUserCommand Nothing command st
    msg         -> executeChat msg st

-- | Execute command provided by user, resolve aliases if necessary.
executeUserCommand :: Maybe Text -> String -> ClientState -> IO CommandResult
executeUserCommand discoTime command st = do
  let key = Text.takeWhile (/=' ') tcmd
      rest = dropWhile (==' ') . dropWhile (/=' ') $ command

  case views (clientConfig . configMacros) (recognize key) st of
    Exact (FreeformMacro cmdExs) ->
      case traverse resolveMacro cmdExs of
        Nothing   -> commandFailureMsg "Macro expansions failed" st
        Just cmds -> process cmds st
    Exact (SpecMacro spec cmds) ->
      case parseArguments spec rest of
        Nothing -> commandFailure st
        Just args -> let env = mkMacroEnv discoTime st args in
          process (($env) <$> cmds) st
    _ ->  executeCommand Nothing command st
  where
    resolveMacro = resolveMacroExpansions (commandExpansion discoTime st) expandInt

    tcmd = Text.pack command
    args = Text.words tcmd

    expandInt i = preview (ix (fromInteger i)) args

    process [] st0 = commandSuccess st0
    process (c:cs) st0 =
      do res <- executeCommand Nothing (Text.unpack c) st0
         case res of
           CommandSuccess st1 -> process cs st1
           CommandFailure st1 -> process cs st1 -- ?
           CommandQuit st1    -> return (CommandQuit st1)

-- Makes the environment for calling a macro from the client state and other
-- information.
mkMacroEnv
 :: Maybe Text  -- ^ disconnectTime
 -> ClientState -- ^ client state
 -> a           -- ^ arguments
 -> MacroEnv a
mkMacroEnv discoTime st args =
  MacroEnv
    { network    = commandExpansion discoTime st "network"
    , channel    = commandExpansion discoTime st "channel"
    , nick       = commandExpansion discoTime st "nick"
    , disconnect = discoTime
    , arguments  = args
    }

-- | Compute the replacement value for the given expansion variable.
commandExpansion ::
  Maybe Text  {- ^ disconnect time    -} ->
  ClientState {- ^ client state       -} ->
  Text        {- ^ expansion variable -} ->
  Maybe Text  {- ^ expansion value    -}
commandExpansion discoTime st v =
  case v of
    "network" -> views clientFocus focusNetwork st
    "channel" -> previews (clientFocus . _ChannelFocus . _2) idText st
    "nick"    -> do net <- views clientFocus focusNetwork st
                    cs  <- preview (clientConnection net) st
                    return (views csNick idText cs)
    "disconnect" -> discoTime
    _         -> Nothing


-- | Respond to the TAB key being pressed. This can dispatch to a command
-- specific completion mode when relevant. Otherwise this will complete
-- input based on the users of the channel related to the current buffer.
tabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
tabCompletion isReversed st =
  case snd $ clientLine st of
    '/':command -> executeCommand (Just isReversed) command st
    _           -> nickTabCompletion isReversed st

-- | Treat the current text input as a chat message and send it.
executeChat :: String -> ClientState -> IO CommandResult
executeChat msg st =
  case view clientFocus st of
    ChannelFocus network channel
      | Just !cs <- preview (clientConnection network) st ->
          do now <- getZonedTime
             let msgTxt = Text.pack $ takeWhile (/='\n') msg
                 ircMsg = ircPrivmsg channel msgTxt
                 myNick = UserInfo (view csNick cs) "" ""
                 entry = ClientMessage
                            { _msgTime    = now
                            , _msgNetwork = network
                            , _msgBody    = IrcBody (Privmsg myNick channel msgTxt)
                            }
             sendMsg cs ircMsg
             commandSuccess $ recordChannelMessage network channel entry st

    _ -> commandFailureMsg "This command requires an active channel" st


-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand :: Maybe Bool -> String -> ClientState -> IO CommandResult

executeCommand (Just isReversed) _ st
  | Just st' <- commandNameCompletion isReversed st = commandSuccess st'

executeCommand tabCompleteReversed str st =
  let (cmd, rest) = break (==' ') str
      cmdTxt      = Text.toLower (Text.pack cmd)

      finish spec exec tab =
        case tabCompleteReversed of
          Just isReversed -> tab isReversed st rest
          Nothing ->
            case parseArguments spec rest of
              Nothing -> commandFailure st
              Just arg -> exec st arg
  in
  case recognize cmdTxt commands of

    Exact (Command argSpec _docs impl) ->
      case impl of
        ClientCommand exec tab ->
          finish argSpec exec tab

        NetworkCommand exec tab
          | Just network <- views clientFocus focusNetwork st
          , Just cs      <- preview (clientConnection network) st ->
              finish argSpec (exec cs) (\x -> tab x cs)
          | otherwise -> commandFailureMsg "This command requires an active network" st

        ChannelCommand exec tab
          | ChannelFocus network channelId <- view clientFocus st
          , Just cs <- preview (clientConnection network) st
          , isChannelIdentifier cs channelId ->
              finish argSpec (exec channelId cs) (\x -> tab x channelId cs)
          | otherwise -> commandFailureMsg "This command requires an active channel" st

        ChatCommand exec tab
          | ChannelFocus network channelId <- view clientFocus st
          , Just cs <- preview (clientConnection network) st ->
              finish argSpec (exec channelId cs) (\x -> tab x channelId cs)
          | otherwise -> commandFailureMsg "This command requires an active chat window" st

    _ -> case tabCompleteReversed of
           Just isReversed -> nickTabCompletion isReversed st
           Nothing         -> commandFailureMsg "Unknown command" st


-- | Expands each alias to have its own copy of the command callbacks
expandAliases :: [(NonEmpty a,b)] -> [(a,b)]
expandAliases xs = [ (a,b) | (as,b) <- xs, a <- toList as ]


-- | Map of built-in client commands to their implementations, tab completion
-- logic, and argument structures.
commands :: Recognizer Command
commands = fromCommands (expandAliases commandsList)

commandsList :: [(NonEmpty Text,Command)]
commandsList =
  --
  -- Client commands
  --
  [ ( pure "connect"
    , Command (ReqTokenArg "network" NoArg)
      "Connect to \^Bnetwork\^B by name.\n\
      \\n\
      \If no name is configured the hostname is the 'name'.\n"
    $ ClientCommand cmdConnect tabConnect
    )
  , ( pure "exit"
    , Command NoArg
      "Exit the client immediately.\n"
    $ ClientCommand cmdExit noClientTab
    )
  , ( pure "focus"
    , Command (ReqTokenArg "network" (OptTokenArg "channel" NoArg))
      "Change the focused window.\n\
      \\n\
      \When only \^Bnetwork\^B is specified this switches to the network status window.\n\
      \When \^Bnetwork\^B and \^Bchannel\^B are specified this switches to that chat window.\n\
      \\n\
      \Nicknames can be specified in the \^Bchannel\^B parameter to switch to private chat.\n\
      \See also: /channel (aliased /c) to switch to a channel on the current network.\n"
    $ ClientCommand cmdFocus tabFocus
    )
  , ( pure "clear"
    , Command (OptTokenArg "network" (OptTokenArg "channel" NoArg))
      "Clear a window.\n\
      \\n\
      \If no arguments are provided the current window is cleared.\n\
      \If \^Bnetwork\^B is provided the that network window is cleared.\n\
      \If \^Bnetwork\^B and \^Bchannel\^B are provided that chat window is cleared.\n\
      \\n\
      \If a window is cleared and no longer active that window will be removed from the client.\n"
    $ ClientCommand cmdClear noClientTab
    )
  , ( pure "reconnect"
    , Command NoArg
      "Reconnect to the current network.\n"
    $ ClientCommand cmdReconnect noClientTab
    )
  , ( pure "ignore"
    , Command (RemainingArg "nicks")
      "Toggle the soft-ignore on each of the space-delimited given nicknames.\n"
    $ ClientCommand cmdIgnore simpleClientTab
    )
  , ( pure "reload"
    , Command (OptTokenArg "filename" NoArg)
      "Reload the client configuration file.\n\
      \\n\
      \If \^Bfilename\^B is provided it will be used to reload.\n\
      \Otherwise the previously loaded configuration file will be reloaded.\n"
    $ ClientCommand cmdReload tabReload
    )
  , ( pure "extension"
    , Command (ReqTokenArg "extension" (RemainingArg "arguments"))
      "Calls the process_command callback of the given extension.\n\
      \\n\
      \\^Bextension\^B should be the name of the loaded extension.\n"
    $ ClientCommand cmdExtension simpleClientTab
    )
  , ( pure "windows"
    , Command NoArg
      "Show a list of all client message windows.\n"
    $ ClientCommand cmdWindows noClientTab
    )
  , ( pure "palette"
    , Command NoArg
      "Show the current palette settings and a color chart to help pick new colors.\n"
    $ ClientCommand cmdPalette noClientTab
    )
  , ( pure "exec"
    , Command (RemainingArg "arguments")
      "Execute a command synchnonously sending the to a configuration destination.\n\
      \\n\
      \\^Barguments\^B: [-n network] [-c channel] [-i input] command [command arguments...]\n\
      \\n\
      \When \^Binput\^B is specified it is sent to the stdin.\n\
      \\n\
      \When neither \^Bnetwork\^B nor \^Bchannel\^B are specified output goes to client window (*)\n\
      \When \^Bnetwork\^B is specified output is sent as raw IRC traffic to the network.\n\
      \When \^Bchannel\^B is specified output is sent as chat to the given channel on the current network.\n\
      \When \^Bnetwork\^B and \^Bchannel\^B are specified output is sent as chat to the given channel on the given network.\n\
      \\n\
      \\^Barguments\^B is divided on spaces into words before being processed\
      \ by getopt. Use Haskell string literal syntax to create arguments with\
      \ escaped characters and spaces inside.\n\
      \\n"
    $ ClientCommand cmdExec simpleClientTab
    )
  , ( pure "url"
    , Command (OptTokenArg "number" NoArg)
      "Open a URL seen in chat.\n\
      \\n\
      \The URL is opened using the executable configured under \^Burl-opener\^B.\n\
      \\n\
      \When this command is active in the textbox, chat messages are filtered to only show ones with URLs.\n\
      \\n\
      \When \^Bnumber\^B is omitted it defaults to \^B1\^B. The number selects the URL to open counting back from the most recent.\n"
    $ ClientCommand cmdUrl noClientTab
    )
  , ( pure "help"
    , Command (OptTokenArg "command" NoArg)
      "Show command documentation.\n\
      \\n\
      \When \^Bcommand\^B is omitted a list of all commands is displayed.\n\
      \When \^Bcommand\^B is specified detailed help for that command is shown.\n"
    $ ClientCommand cmdHelp tabHelp
    )

  --
  -- Network commands
  --
  , ( pure "quote"
    , Command (RemainingArg "raw IRC command")
      "Send a raw IRC command.\n"
    $ NetworkCommand cmdQuote  simpleNetworkTab
    )
  , ( "join" :| ["j"]
    , Command (ReqTokenArg "channels" (OptTokenArg "keys" NoArg))
      "Join a chat channel.\n\
      \\n\
      \\^Bchannels\^B: comma-separated list of channels\n\
      \\^Bkeys\^B: comma-separated list of keys\n"
    $ NetworkCommand cmdJoin   simpleNetworkTab
    )
  , ( "channel" :| ["c"]
    , Command (ReqTokenArg "channel" NoArg)
      "Change the focused window.\n\
      \\n\
      \Changes the focus to the \^Bchannel\^B chat window on the current network.\n\
      \\n\
      \Nicknames can be specified in the \^Bchannel\^B parameter to switch to private chat.\n\
      \See also: /focus to switch to a channel on a different network.\n\
      \See also: /focus to switch to a channel on a different network.\n"
    $ NetworkCommand cmdChannel simpleNetworkTab
    )
  , ( pure "mode"
    , Command (RemainingArg "modes and parameters")
      "Sets IRC modes.\n\
      \\n\
      \Examples:\n\
      \Setting a ban:           /mode +b *!*@hostname\n\
      \Removing a quiet:        /mode -q *!*@hostname\n\
      \Voicing two users:       /mode +vv user1 user2\n\
      \Demoting an op to voice: /mode +v-o user1 user1\n\
      \\n\
      \When executed in a network window, mode changes are applied to your user.\n\
      \When executed in a channel window, mode changes are applied to the channel.\n\
      \\n\
      \This command has parameter sensitive tab-completion.\n"
    $ NetworkCommand cmdMode   tabMode
    )
  , ( pure "msg"
    , Command (ReqTokenArg "target" (RemainingArg "message"))
      "Send a chat message to a user or a channel.\n\
      \\n\
      \\^Btarget\^B can be a channel or nickname.\n"
    $ NetworkCommand cmdMsg    simpleNetworkTab
    )
  , ( pure "notice"
    , Command (ReqTokenArg "target" (RemainingArg "message"))
      "Send a notice message to a user or a channel. Notices are typically used by bots.\n\
      \\n\
      \\^Btarget\^B can be a channel or nickname.\n"
    $ NetworkCommand cmdNotice simpleNetworkTab
    )
  , ( pure "ctcp"
    , Command (ReqTokenArg "target" (ReqTokenArg "command" (RemainingArg "arguments")))
      "Send a CTCP command to a user or a channel.\n\
      \\n\
      \Examples:\n\
      \Version query:    /ctcp user1 version\n\
      \Local-time query: /ctcp user1 time\n\
      \\n\
      \\^Btarget\^B can be a channel or nickname.\n\
      \\^Bcommand\^B can be any CTCP command.\n\
      \\^Barguments\^B are specific to a particular command.\n"
    $ NetworkCommand cmdCtcp simpleNetworkTab
    )
  , ( pure "nick"
    , Command (ReqTokenArg "nick" NoArg)
      "Change your nickname.\n"
    $ NetworkCommand cmdNick   simpleNetworkTab
    )
  , ( pure "quit"
    , Command (RemainingArg "reason")
      "Gracefully disconnect the current network connection.\n\
      \\n\
      \\^Breason\^B: optional quit reason\n\
      \\n\
      \See also: /disconnect /exit\n"
    $ NetworkCommand cmdQuit   simpleNetworkTab
    )
  , ( pure "disconnect"
    , Command NoArg
      "Immediately terminate the current network connection.\n\
      \\n\
      \See also: /quit /exit\n"
    $ NetworkCommand cmdDisconnect noNetworkTab
    )
  , ( pure "who"
    , Command (RemainingArg "arguments")
      "Send WHO query to server with given arguments.\n"
    $ NetworkCommand cmdWho simpleNetworkTab
    )
  , ( pure "whois"
    , Command (RemainingArg "arguments")
      "Send WHOIS query to server with given arguments.\n"
    $ NetworkCommand cmdWhois simpleNetworkTab
    )
  , ( pure "whowas"
    , Command (RemainingArg "arguments")
      "Send WHOWAS query to server with given arguments.\n"
    $ NetworkCommand cmdWhowas simpleNetworkTab
    )
  , ( pure "ison"
    , Command (RemainingArg "arguments")
      "Send ISON query to server with given arguments.\n"
    $ NetworkCommand cmdIson   simpleNetworkTab
    )
  , ( pure "userhost"
    , Command (RemainingArg "arguments")
      "Send USERHOST query to server with given arguments.\n"
    $ NetworkCommand cmdUserhost simpleNetworkTab
    )
  , ( pure "away"
    , Command (RemainingArg "message")
      "Set away status.\n\
      \\n\
      \When \^Bmessage\^B is omitted away status is cleared.\n"
    $ NetworkCommand cmdAway   simpleNetworkTab
    )
  , ( pure "links"
    , Command (RemainingArg "arguments")
      "Send LINKS query to server with given arguments.\n"
    $ NetworkCommand cmdLinks  simpleNetworkTab
    )
  , ( pure "time"
    , Command (RemainingArg "arguments")
      "Send TIME query to server with given arguments.\n"
    $ NetworkCommand cmdTime   simpleNetworkTab
    )
  , ( pure "stats"
    , Command (RemainingArg "arguments")
      "Send STATS query to server with given arguments.\n"
    $ NetworkCommand cmdStats  simpleNetworkTab
    )
  , ( pure "znc"
    , Command (RemainingArg "arguments")
      "Send command directly to ZNC.\n\
      \\n\
      \The advantage of this over /msg is that responses are not broadcast to call clients.\n"
    $ NetworkCommand cmdZnc    simpleNetworkTab
    )
  , ( pure "znc-playback"
    , Command (OptTokenArg "time" (OptTokenArg "date" NoArg))
      "Request playback from the ZNC 'playback' module.\n\
      \\n\
      \\^Btime\^B determines the time to playback since.\n\
      \\^Bdate\^B determines the date to playback since.\n\
      \\n\
      \When both \^Btime\^B and \^Bdate\^B are omitted, all playback is requested.\n\
      \When both \^Bdate\^B is omitted it is defaulted the most recent date in the past that makes sense.\n\
      \\n\
      \Time format: HOURS:MINUTES (example: 7:00)\n\
      \Date format: YEAR-MONTH-DAY (example: 2016-06-16)\n\
      \\n\
      \Note that the playback module is not installed in ZNC by default!\n"
    $ NetworkCommand cmdZncPlayback noNetworkTab
    )

  , ( pure "invite"
    , Command (ReqTokenArg "nick" NoArg)
      "Invite a user to the current channel.\n"
    $ ChannelCommand cmdInvite simpleChannelTab
    )
  , ( pure "topic"
    , Command (RemainingArg "message")
      "Set the topic on the current channel.\n\
      \\n\
      \Tab-completion with no \^Bmessage\^B specified will load the current topic for editing.\n"
    $ ChannelCommand cmdTopic tabTopic
    )
  , ( pure "kick"
    , Command (ReqTokenArg "nick" (RemainingArg "reason"))
      "Kick a user from the current channel.\n\
      \\n\
      \See also: /kickban /remove\n"
    $ ChannelCommand cmdKick   simpleChannelTab
    )
  , ( pure "kickban"
    , Command (ReqTokenArg "nick" (RemainingArg "reason"))
      "Ban and kick a user from the current channel.\n\
      \\n\
      \Users are banned by hostname match.\n\
      \See also: /kick /remove\n"
    $ ChannelCommand cmdKickBan simpleChannelTab
    )
  , ( pure "remove"
    , Command (ReqTokenArg "nick" (RemainingArg "reason"))
      "Remove a user from the current channel.\n\
      \\n\
      \Remove works like /kick except it results in a PART.\n\
      \See also: /kick /kickban\n"
    $ ChannelCommand cmdRemove simpleChannelTab
    )
  , ( pure "part"
    , Command (RemainingArg "reason")
      "Part from the current channel.\n"
    $ ChannelCommand cmdPart simpleChannelTab
    )

  , ( pure "users"
    , Command NoArg
      "Show the user list for the current channel.\n\
      \\n\
      \Detailed view (F2) shows full hostmask.\n\
      \Hostmasks can be populated with /who #channel.\n"
    $ ChannelCommand cmdUsers  noChannelTab
    )
  , ( pure "channelinfo"
    , Command NoArg
      "Show information about the current channel.\n"
    $ ChannelCommand cmdChannelInfo noChannelTab
    )
  , ( pure "masks"
    , Command (ReqTokenArg "mode" NoArg)
      "Show mask lists for current channel.\n\
      \\n\
      \Common \^Bmode\^B values:\n\
      \\^Bb\^B: bans\n\
      \\^Bq\^B: quiets\n\
      \\^BI\^B: invite exemptions (op view only)\n\
      \\^Be\^B: ban exemption (op view only)s\n\
      \\n\
      \To populate the mask lists for the first time use: /mode \^Bmode\^B\n"
    $ ChannelCommand cmdMasks noChannelTab
    )

  , ( pure "me"
    , Command (RemainingArg "message")
      "Send an 'action' to the current chat window.\n"
    $ ChatCommand cmdMe simpleChannelTab
    )
  , ( pure "say"
    , Command (RemainingArg "message")
      "Send a message to the current chat window.\n\
      \\n\
      \This can be useful for sending a chat message with a leading '/'.\n"
    $ ChatCommand cmdSay simpleChannelTab
    )
  ]

-- | Provides no tab completion for client commands
noClientTab :: Bool -> ClientCommand String
noClientTab _ st _ = commandFailure st

-- | Provides no tab completion for network commands
noNetworkTab :: Bool -> NetworkCommand String
noNetworkTab _ _ st _ = commandFailure st

-- | Provides no tab completion for channel commands
noChannelTab :: Bool -> ChannelCommand String
noChannelTab _ _ _ st _ = commandFailure st

-- | Provides nickname based tab completion for client commands
simpleClientTab :: Bool -> ClientCommand String
simpleClientTab isReversed st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for network commands
simpleNetworkTab :: Bool -> NetworkCommand String
simpleNetworkTab isReversed _ st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for channel commands
simpleChannelTab :: Bool -> ChannelCommand String
simpleChannelTab isReversed _ _ st _ =
  nickTabCompletion isReversed st

-- | Implementation of @/exit@ command.
cmdExit :: ClientCommand ()
cmdExit st _ = return (CommandQuit st)

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: ClientCommand (Maybe (String, Maybe (String, ())))
cmdClear st args =
  case args of
    Nothing                 -> clearFocus (view clientFocus st)
    Just ("*", Nothing)     -> clearFocus Unfocused
    Just (network, Nothing) -> clearFocus (NetworkFocus (Text.pack network))
    Just (network, Just (channel, _)) ->
        clearFocus (ChannelFocus (Text.pack network) (mkId (Text.pack channel)))
  where
    clearFocus focus = commandSuccess (windowEffect st)
      where
        windowEffect
          | isActive  = clearWindow
          | otherwise = deleteWindow

        deleteWindow = advanceFocus . setWindow Nothing
        clearWindow  =                setWindow (Just emptyWindow)

        setWindow = set (clientWindows . at (view clientFocus st))

        isActive =
          case focus of
            Unfocused                    -> False
            NetworkFocus network         -> has (clientConnection network) st
            ChannelFocus network channel -> has (clientConnection network
                                                .csChannels . ix channel) st

-- | Implementation of @/quote@. Parses arguments as a raw IRC command and
-- sends to the current network.
cmdQuote :: NetworkCommand String
cmdQuote cs st rest =
  case parseRawIrcMsg (Text.pack rest) of
    Nothing  -> commandFailureMsg "Failed to parse IRC command" st
    Just raw ->
      do sendMsg cs raw
         commandSuccess st

-- | Implementation of @/me@
cmdMe :: ChannelCommand String
cmdMe channelId cs st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         !myNick = UserInfo (view csNick cs) "" ""
         network = view csNetwork cs
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Ctcp myNick channelId "ACTION" (Text.pack rest))
                    }
     sendMsg cs (ircPrivmsg channelId actionTxt)
     commandSuccess
       $ recordChannelMessage network channelId entry st

-- | Implementation of @/ctcp@
cmdCtcp :: NetworkCommand (String, (String, String))
cmdCtcp cs st (target, (cmd, args)) =
  do let cmdTxt = Text.toUpper (Text.pack cmd)
         argTxt = Text.pack args
         tgtTxt = Text.pack target

     sendMsg cs (ircPrivmsg (mkId tgtTxt) ("\^A" <> cmdTxt <> " " <> argTxt <> "\^A"))
     chatCommand
        (\src tgt -> Ctcp src tgt cmdTxt argTxt)
        tgtTxt cs st

-- | Implementation of @/notice@
cmdNotice :: NetworkCommand (String, String)
cmdNotice cs st (target, rest)
  | null rest = commandFailure st
  | otherwise =
      do let restTxt = Text.pack rest
             tgtTxt = Text.pack target

         sendMsg cs (ircNotice (mkId tgtTxt) restTxt)
         chatCommand
            (\src tgt -> Notice src tgt restTxt)
            tgtTxt cs st

-- | Implementation of @/msg@
cmdMsg :: NetworkCommand (String, String)
cmdMsg cs st (target, rest)
  | null rest = commandFailure st
  | otherwise =
      do let restTxt = Text.pack rest
             tgtTxt = Text.pack target

         sendMsg cs (ircPrivmsg (mkId tgtTxt) restTxt)
         chatCommand
            (\src tgt -> Privmsg src tgt restTxt)
            tgtTxt cs st

-- | Common logic for @/msg@ and @/notice@
chatCommand ::
  (UserInfo -> Identifier -> IrcMsg) ->
  Text {- ^ target  -} ->
  NetworkState         ->
  ClientState          ->
  IO CommandResult
chatCommand mkmsg target cs st =
  commandSuccess =<< chatCommand' mkmsg target cs st

-- | Common logic for @/msg@ and @/notice@ returning the client state
chatCommand' ::
  (UserInfo -> Identifier -> IrcMsg) ->
  Text {- ^ target  -} ->
  NetworkState         ->
  ClientState          ->
  IO ClientState
chatCommand' con targetsTxt cs st =
  do now <- getZonedTime
     let targetTxts = Text.split (==',') targetsTxt
         targetIds  = mkId <$> targetTxts
         !myNick = UserInfo (view csNick cs) "" ""
         network = view csNetwork cs
         entries = [ (targetId,
                          ClientMessage
                          { _msgTime = now
                          , _msgNetwork = network
                          , _msgBody = IrcBody (con myNick targetId)
                          })
                       | targetId <- targetIds ]

     return $! foldl' (\acc (targetId, entry) ->
                         recordChannelMessage network targetId entry acc)
                      st
                      entries


cmdConnect :: ClientCommand (String, ())
cmdConnect st (networkStr, _) =
  do -- abort any existing connection before connecting
     let network = Text.pack networkStr
     st' <- addConnection 0 Nothing network =<< abortNetwork network st
     commandSuccess
       $ changeFocus (NetworkFocus network) st'

cmdFocus :: ClientCommand (String, Maybe (String, ()))
cmdFocus st (network, mbChannel)
  | network == "*" = commandSuccess (changeFocus Unfocused st)
  | otherwise =
     case mbChannel of
       Nothing ->
         let focus = NetworkFocus (Text.pack network) in
         commandSuccess (changeFocus focus st)
       Just (channel,_) ->
         let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
         commandSuccess
           $ changeFocus focus st

-- | Implementation of @/windows@ command. Set subfocus to Windows.
cmdWindows :: ClientCommand ()
cmdWindows st _ = commandSuccess (changeSubfocus FocusWindows st)

-- | Implementation of @/palette@ command. Set subfocus to Windows.
cmdPalette :: ClientCommand ()
cmdPalette st _ = commandSuccess (changeSubfocus FocusPalette st)

-- | Implementation of @/help@ command. Set subfocus to Windows.
cmdHelp :: ClientCommand (Maybe (String, ()))
cmdHelp st mb = commandSuccess (changeSubfocus focus st)
  where
    focus = FocusHelp (fmap (Text.pack . fst) mb)

tabHelp :: Bool -> ClientCommand String
tabHelp isReversed st _ = simpleTabCompletion id [] commandNames isReversed st
  where
    commandNames = [ cmd | (cmd :| _, _) <- commandsList ]

simpleTabCompletion ::
  Prefix a =>
  (String -> String) {- ^ leading transform -} ->
  [a] {- ^ hints           -} ->
  [a] {- ^ all completions -} ->
  Bool {- ^ reversed order -} ->
  ClientState -> IO CommandResult
simpleTabCompletion lead hints completions isReversed st =
  case traverseOf clientTextBox tryCompletion st of
    Nothing  -> commandFailure st
    Just st' -> commandSuccess st'
  where
    tryCompletion = wordComplete lead isReversed hints completions

-- | @/connect@ tab completes known server names
tabConnect :: Bool -> ClientCommand String
tabConnect isReversed st _ =
  simpleTabCompletion id [] networks isReversed st
  where
    networks = views clientNetworkMap               HashMap.keys st
            ++ views (clientConfig . configServers) HashMap.keys st


-- | When tab completing the first parameter of the focus command
-- the current networks are used.
tabFocus :: Bool -> ClientCommand String
tabFocus isReversed st _ =
  simpleTabCompletion id [] completions isReversed st
  where
    networks   = map mkId $ HashMap.keys $ view clientNetworkMap st
    params     = words $ uncurry take $ clientLine st

    completions
      | length params == 2 = networks
      | otherwise          = currentCompletionList st

cmdWhois :: NetworkCommand String
cmdWhois cs st rest =
  do sendMsg cs (ircWhois (Text.pack <$> words rest))
     commandSuccess st

cmdWho :: NetworkCommand String
cmdWho cs st rest =
  do sendMsg cs (ircWho (Text.pack <$> words rest))
     commandSuccess st

cmdWhowas :: NetworkCommand String
cmdWhowas cs st rest =
  do sendMsg cs (ircWhowas (Text.pack <$> words rest))
     commandSuccess st

cmdIson :: NetworkCommand String
cmdIson cs st rest =
  do sendMsg cs (ircIson (Text.pack <$> words rest))
     commandSuccess st

cmdUserhost :: NetworkCommand String
cmdUserhost cs st rest =
  do sendMsg cs (ircUserhost (Text.pack <$> words rest))
     commandSuccess st

cmdStats :: NetworkCommand String
cmdStats cs st rest =
  do sendMsg cs (ircStats (Text.pack <$> words rest))
     commandSuccess st

cmdAway :: NetworkCommand String
cmdAway cs st rest =
  do sendMsg cs (ircAway (Text.pack rest))
     commandSuccess st

cmdLinks :: NetworkCommand String
cmdLinks cs st rest =
  do sendMsg cs (ircLinks (Text.pack <$> words rest))
     commandSuccess st

cmdTime :: NetworkCommand String
cmdTime cs st rest =
  do sendMsg cs (ircTime (Text.pack <$> words rest))
     commandSuccess st

cmdZnc :: NetworkCommand String
cmdZnc cs st rest =
  do sendMsg cs (ircZnc (Text.words (Text.pack rest)))
     commandSuccess st

cmdZncPlayback :: NetworkCommand (Maybe (String, Maybe (String, ())))
cmdZncPlayback cs st args =
  case args of

    -- request everything
    Nothing -> success "0"

    -- current date explicit time
    Just (timeStr, Nothing)
       | Just tod <- parse timeFormats timeStr ->
          do now <- getZonedTime
             let (nowTod,t) = (zonedTimeLocalTime . localTimeTimeOfDay <<.~ tod) now
                 yesterday = over (zonedTimeLocalTime . localTimeDay) (addDays (-1))
                 fixDay
                   | tod <= nowTod = id
                   | otherwise     = yesterday
             successZoned (fixDay t)

    -- explicit date and time
    Just (dateStr, Just (timeStr, _))
       | Just day  <- parse dateFormats dateStr
       , Just tod  <- parse timeFormats timeStr ->
          do tz <- getCurrentTimeZone
             successZoned ZonedTime
               { zonedTimeZone = tz
               , zonedTimeToLocalTime = LocalTime
                   { localTimeOfDay = tod
                   , localDay       = day } }

    _ -> commandFailureMsg "Unable to parse date/time arguments" st

  where
    -- %k doesn't require a leading 0 for times before 10AM
    timeFormats = ["%k:%M:%S","%k:%M"]
    dateFormats = ["%F"]
    parse formats str =
      asum (map (parseTimeM False defaultTimeLocale ?? str) formats)

    successZoned = success . formatTime defaultTimeLocale "%s"

    success start =
      do sendMsg cs (ircZnc ["*playback", "play", "*", Text.pack start])
         commandSuccess st

cmdMode :: NetworkCommand String
cmdMode cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkCommand (String, ())
cmdNick cs st (nick,_) =
  do sendMsg cs (ircNick (mkId (Text.pack nick)))
     commandSuccess st

cmdPart :: ChannelCommand String
cmdPart channelId cs st rest =
  do let msg = rest
     sendMsg cs (ircPart channelId (Text.pack msg))
     commandSuccess st

-- | This command is equivalent to chatting without a command. The primary use
-- at the moment is to be able to send a leading @/@ to chat easily.
cmdSay :: ChannelCommand String
cmdSay _ _ st rest = executeChat rest st

cmdInvite :: ChannelCommand (String, ())
cmdInvite channelId cs st (nick,_) =
  do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
         cmd = ircInvite (Text.pack nick) channelId
     cs' <- if freeTarget
              then cs <$ sendMsg cs cmd
              else sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

commandSuccessUpdateCS :: NetworkState -> ClientState -> IO CommandResult
commandSuccessUpdateCS cs st =
  let networkId = view csNetworkId cs in
  commandSuccess
    $ setStrict (clientConnections . ix networkId) cs st

cmdTopic :: ChannelCommand String
cmdTopic channelId cs st rest =
  do let cmd =
           case rest of
             ""    -> ircTopic channelId ""
             topic | useChanServ channelId cs ->
                        ircPrivmsg "ChanServ"
                          ("TOPIC " <> idText channelId <> Text.pack (' ' : topic))
                   | otherwise -> ircTopic channelId (Text.pack topic)
     sendMsg cs cmd
     commandSuccess st

tabTopic ::
  Bool {- ^ reversed -} ->
  ChannelCommand String
tabTopic _ channelId cs st rest

  | all (==' ') rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = set Edit.line (Edit.endLine $ "/topic " ++ Text.unpack topic)
        commandSuccess (over clientTextBox textBox st)

  | otherwise = commandFailure st


cmdUsers :: ChannelCommand ()
cmdUsers _ _ st _ = commandSuccess (changeSubfocus FocusUsers st)

cmdChannelInfo :: ChannelCommand ()
cmdChannelInfo _ _ st _ = commandSuccess (changeSubfocus FocusInfo st)

cmdMasks :: ChannelCommand (String,())
cmdMasks _ cs st (rest,_) =
  case rest of
    [mode] | mode `elem` view (csModeTypes . modesLists) cs ->
        commandSuccess (changeSubfocus (FocusMasks mode) st)
    _ -> commandFailureMsg "Unknown mask mode" st

cmdKick :: ChannelCommand (String, String)
cmdKick channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircKick channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st


cmdKickBan :: ChannelCommand (String, String)
cmdKickBan channelId cs st (who,reason) =
  do let msg = Text.pack reason

         whoTxt     = Text.pack who

         mask = renderUserInfo (computeBanUserInfo (mkId whoTxt) cs)
         cmds = [ ircMode channelId ["b", mask]
                , ircKick channelId whoTxt msg
                ]
     cs' <- sendModeration channelId cmds cs
     commandSuccessUpdateCS cs' st

computeBanUserInfo :: Identifier -> NetworkState    -> UserInfo
computeBanUserInfo who cs =
  case view (csUser who) cs of
    Nothing                   -> UserInfo who "*" "*"
    Just (UserAndHost _ host) -> UserInfo "*" "*" host

cmdRemove :: ChannelCommand (String, String)
cmdRemove channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircRemove channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

cmdJoin :: NetworkCommand (String, Maybe (String, ()))
cmdJoin cs st (channels, mbKeys) =
  do let network = view csNetwork cs
     let channelId = mkId (Text.pack (takeWhile (/=',') channels))
     sendMsg cs (ircJoin (Text.pack channels) (Text.pack . fst <$> mbKeys))
     commandSuccess
        $ changeFocus (ChannelFocus network channelId) st


-- | @/channel@ command. Takes the name of a channel and switches
-- focus to that channel on the current network.
cmdChannel :: NetworkCommand (String, ())
cmdChannel cs st (channel, _) =
  commandSuccess
    $ changeFocus (ChannelFocus (view csNetwork cs) (mkId (Text.pack channel))) st


cmdQuit :: NetworkCommand String
cmdQuit cs st rest =
  do let msg = Text.pack rest
     sendMsg cs (ircQuit msg)
     commandSuccess st

cmdDisconnect :: NetworkCommand ()
cmdDisconnect cs st _ =
  do st' <- abortNetwork (view csNetwork cs) st
     commandSuccess st'

-- | Reconnect to the currently focused network. It's possible
-- that we're not currently connected to a network, so
-- this is implemented as a client command.
cmdReconnect :: ClientCommand ()
cmdReconnect st _
  | Just network <- views clientFocus focusNetwork st =

      do tm <- getCurrentTime
         st' <- addConnection 0 (Just tm) network =<< abortNetwork network st
         commandSuccess
           $ changeFocus (NetworkFocus network) st'

  | otherwise = commandFailureMsg "/reconnect requires focused network" st

cmdIgnore :: ClientCommand String
cmdIgnore st rest =
  case mkId . Text.pack <$> words rest of
    [] -> commandFailure st
    xs -> commandSuccess
            $ over clientIgnores updateIgnores st
      where
        updateIgnores :: HashSet Identifier -> HashSet Identifier
        updateIgnores s = foldl' updateIgnore s xs
        updateIgnore s x = over (contains x) not s

-- | Implementation of @/reload@
--
-- Attempt to reload the configuration file
cmdReload :: ClientCommand (Maybe (String, ()))
cmdReload st mbPath =
  do let path = fst <$> mbPath
            <|> view (clientConfig . configConfigPath) st
     res <- loadConfiguration path
     case res of
       Left e    -> commandFailureMsg (describeProblem e) st
       Right cfg ->
         do st1 <- clientStartExtensions (set clientConfig cfg st)
            commandSuccess st1

  where
    describeProblem err =
      Text.pack $
      case err of
       ConfigurationReadFailed e  -> "Failed to open configuration:" ++ e
       ConfigurationParseFailed e -> "Failed to parse configuration:" ++ e
       ConfigurationMalformed e   -> "Configuration malformed: " ++ e

-- | Support file name tab completion when providing an alternative
-- configuration file.
--
-- /NOT IMPLEMENTED/
tabReload :: Bool {- ^ reversed -} -> ClientCommand String
tabReload _ st _ = commandFailure st

modeCommand ::
  [Text] {- mode parameters -} ->
  NetworkState                 ->
  ClientState                  ->
  IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (ircMode (view csNick cs) modes)
         commandSuccess st

    ChannelFocus _ chan ->
      case modes of
        [] -> success False [[]]
        flags:params ->
          case splitModes (view csModeTypes cs) flags params of
            Nothing -> commandFailureMsg "Failed to parse modes" st
            Just parsedModes ->
              success needOp (unsplitModes <$> chunksOf (view csModeCount cs) parsedModes')
              where
                parsedModes'
                  | useChanServ chan cs = filter (not . isOpMe) parsedModes
                  | otherwise           = parsedModes

                needOp = not (all isPublicChannelMode parsedModes)
      where
        isOpMe (True, 'o', param) = mkId param == view csNick cs
        isOpMe _                  = False

        success needOp argss =
          do let cmds = ircMode chan <$> argss
             cs' <- if needOp
                      then sendModeration chan cmds cs
                      else cs <$ traverse_ (sendMsg cs) cmds
             commandSuccessUpdateCS cs' st

    _ -> commandFailure st

tabMode :: Bool -> NetworkCommand String
tabMode isReversed cs st rest =
  case view clientFocus st of

    ChannelFocus _ channel
      | flags:params     <- Text.words (Text.pack rest)
      , Just parsedModes <- splitModes (view csModeTypes cs) flags params
      , let parsedModesWithParams =
              [ (pol,mode) | (pol,mode,arg) <- parsedModes, not (Text.null arg) ]
      , (pol,mode):_      <- drop (paramIndex-3) parsedModesWithParams
      , let (hint, completions) = computeModeCompletion pol mode channel cs st
      -> simpleTabCompletion id hint completions isReversed st

    _ -> commandFailure st

  where
    paramIndex = length $ words $ uncurry take $ clientLine st

activeNicks ::
  ClientState ->
  [Identifier]
activeNicks st =
  case view clientFocus st of
    focus@(ChannelFocus network channel) ->
      toListOf
        ( clientWindows    . ix focus
        . winMessages      . folded
        . wlBody           . _IrcBody
        . folding msgActor . to userNick
        . filtered isActive
        . filtered isNotSelf ) st
      where
        isActive n = HashMap.member n userMap
        self = preview ( clientConnection network . csNick ) st
        isNotSelf n = case self of
                        Nothing -> True
                        Just s -> n /= s
        userMap = view ( clientConnection network
                       . csChannels . ix channel
                       . chanUsers) st

    _ -> []

-- | Use the *!*@host masks of users for channel lists when setting list modes
--
-- Use the channel's mask list for removing modes
--
-- Use the nick list otherwise
computeModeCompletion ::
  Bool {- ^ mode polarity -} ->
  Char {- ^ mode          -} ->
  Identifier {- ^ channel -} ->
  NetworkState    ->
  ClientState ->
  ([Identifier],[Identifier]) {- ^ (hint, complete) -}
computeModeCompletion pol mode channel cs st
  | mode `elem` view modesLists modeSettings =
        if pol then ([],usermasks) else ([],masks)
  | otherwise = (activeNicks st, nicks)
  where
    modeSettings = view csModeTypes cs
    nicks = HashMap.keys (view (csChannels . ix channel . chanUsers) cs)

    masks = mkId <$> HashMap.keys (view (csChannels . ix channel . chanLists . ix mode) cs)

    usermasks =
      [ mkId ("*!*@" <> host)
        | nick <- HashMap.keys (view (csChannels . ix channel . chanUsers) cs)
        , UserAndHost _ host <- toListOf (csUsers . ix nick) cs
        ]

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete id isReversed [] possibilities) st
  where
    n = length leadingPart
    (cursorPos, line) = clientLine st
    leadingPart = takeWhile (/=' ') line
    possibilities = Text.cons '/' <$> commandNames
    commandNames = keys commands
                ++ keys (view (clientConfig . configMacros) st)

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
nickTabCompletion isReversed st =
  simpleTabCompletion (++": ") hint completions isReversed st
  where
    hint          = activeNicks st
    completions   = currentCompletionList st

-- | Used to send commands that require ops to perform.
-- If this channel is one that the user has chanserv access and ops are needed
-- then ops are requested and the commands are queued, otherwise send them
-- directly.
sendModeration ::
  Identifier      {- ^ channel       -} ->
  [RawIrcMsg]     {- ^ commands      -} ->
  NetworkState    {- ^ network state -} ->
  IO NetworkState
sendModeration channel cmds cs
  | useChanServ channel cs =
      do sendMsg cs (ircPrivmsg "ChanServ" ("OP " <> idText channel))
         return $ csChannels . ix channel . chanQueuedModeration <>~ cmds $ cs
  | otherwise = cs <$ traverse_ (sendMsg cs) cmds

useChanServ :: Identifier -> NetworkState    -> Bool
useChanServ channel cs =
  channel `elem` view (csSettings . ssChanservChannels) cs &&
  not (iHaveOp channel cs)

cmdExtension :: ClientCommand (String, String)
cmdExtension st (name,params) =
  case find (\ae -> aeName ae == Text.pack name)
            (view (clientExtensions . esActive) st) of
        Nothing -> commandFailureMsg "Unknown extension" st
        Just ae ->
          do (st',_) <- clientPark st $ \ptr ->
                          commandExtension ptr (Text.pack <$> words params) ae
             commandSuccess st'

-- | Implementation of @/exec@ command.
cmdExec :: ClientCommand String
cmdExec st rest =
  do now <- getZonedTime
     case parseExecCmd rest of
       Left es -> failure now es
       Right ec ->
         case buildTransmitter now ec of
           Left es -> failure now es
           Right tx ->
             do res <- runExecCmd ec
                case res of
                  Left es -> failure now es
                  Right msgs -> tx (map Text.pack msgs)

  where
    buildTransmitter now ec =
      case (Text.pack <$> view execOutputNetwork ec,
            Text.pack <$> view execOutputChannel ec) of
        (Nothing, Nothing) -> Right (sendToClient now)
        (Just network, Nothing) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToNetwork now cs)
        (Nothing , Just channel) ->
          case currentNetworkState of
            Nothing -> Left ["No current network"]
            Just cs -> Right (sendToChannel cs channel)
        (Just network, Just channel) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToChannel cs channel)

    sendToClient now msgs = commandSuccess $! foldl' (recordSuccess now) st msgs

    sendToNetwork now cs msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
           case parseRawIrcMsg msg of
             Nothing ->
               return $! recordError now st1 ("Bad raw message: " <> msg)
             Just raw ->
               do sendMsg cs raw
                  return st1) st msgs

    sendToChannel cs channel msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
        do sendMsg cs (ircPrivmsg (mkId channel) msg)
           chatCommand'
              (\src tgt -> Privmsg src tgt msg)
              channel
              cs st1) st (filter (not . Text.null) msgs)

    currentNetworkState =
      do network <- views clientFocus focusNetwork st
         preview (clientConnection network) st

    failure now es =
      commandFailure $! foldl' (recordError now) st (map Text.pack es)

recordError :: ZonedTime -> ClientState -> Text -> ClientState
recordError now ste e =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgBody    = ErrorBody e
    , _msgNetwork = ""
    } ste

recordSuccess :: ZonedTime -> ClientState -> Text -> ClientState
recordSuccess now ste m =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgBody    = NormalBody m
    , _msgNetwork = ""
    } ste

cmdUrl :: ClientCommand (Maybe (String, ()))
cmdUrl st mbArg =
  case view (clientConfig . configUrlOpener) st of
    Nothing -> commandFailureMsg "/url requires url-opener to be configured" st
    Just opener ->
      case mbArg of
        Nothing -> doUrlOpen opener 0
        Just (arg,_) ->
          case readMaybe arg of
            Just n | n > 0 -> doUrlOpen opener (n-1)
            _ -> commandFailureMsg "/url expected positive integer argument" st
  where
    focus = view clientFocus st

    urlMatches :: Text -> [Text]
    urlMatches = getAllTextMatches . match urlPattern

    urls = toListOf ( clientWindows . ix focus . winMessages . folded . wlText
                    . folding urlMatches) st

    doUrlOpen opener n =
      case preview (ix n) urls of
        Just url -> openUrl opener (Text.unpack url) st
        Nothing  -> commandFailureMsg "/url couldn't find requested URL" st

openUrl :: FilePath -> String -> ClientState -> IO CommandResult
openUrl opener url st =
  do res <- try (callProcess opener [url])
     case res of
       Left e  -> commandFailureMsg (Text.pack (displayException (e :: IOError))) st
       Right{} -> commandSuccess st
