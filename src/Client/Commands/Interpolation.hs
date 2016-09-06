{-# Language OverloadedStrings, GADTs #-}

{-|
Module      : Client.Commands.Interpolation
Description : Parser and evaluator for string interpolation in commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is able to parse commands with inline variables and then
to evaluate those variables to produce a complete command that varies
by the current context.
-}
module Client.Commands.Interpolation
  ( ExpansionChunk(..)
  , parseExpansion
  , resolveMacroExpansions
  , presolveMacroExpansions
  , Macro(..)
  , MacroSpec(..)
  , MacroCommand
  , MacroEnv(..)
  , parseMacroSpecs
  , noMacroArguments
  ) where

import           Control.Applicative
import           Control.Monad (join)
import           Control.Lens ((<&>))
import           Data.Attoparsec.Text as P
import           Data.Bifunctor
import           Data.Char
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Text (Text)

import           Client.Commands.Arguments

-- | Parsed chunk of an expandable command
data ExpansionChunk
  -- | regular text
  = LiteralChunk Text
  -- | inline variable @$x@ or @${x y}@
  | VariableChunk Text
  -- | inline variable @$1@ or @${1}@
  | IntegerChunk Integer
  -- | bracketed variable with default @${x|lit}@
  | DefaultChunk ExpansionChunk Text
  deriving Show

data Macro where
  SpecMacro :: ArgumentSpec s -> [MacroCommand s] -> Macro
  FreeformMacro :: [[ExpansionChunk]] -> Macro

instance Show Macro where
  showsPrec p (SpecMacro spec _)
    = showParen (p >= 11)
    $ showString "SpecMacro " . showsPrec 11 spec . showString " ..."

  showsPrec p (FreeformMacro chunks)
    = showParen (p >= 11)
    $ showString "FreeformMacro " . showsPrec 11 chunks

-- An environment in which a macro can be run. Contains the 
data MacroEnv a
  = MacroEnv
  { network    :: Maybe Text
  , channel    :: Maybe Text
  , nick       :: Maybe Text
  , disconnect :: Maybe Text
  , arguments  :: a
  }

type MacroCommand a = MacroEnv a -> Text

data MacroSpec where
  MacroSpec :: ArgumentSpec s -> MacroSpec

instance Show MacroSpec where
  showsPrec p (MacroSpec as)
    = showParen (p >= 11)
    $ showString "MacroSpec " . showsPrec 11 as

-- | Specification used when unspecified, no arguments.
noMacroArguments :: MacroSpec
noMacroArguments = MacroSpec (RemainingArg "")

parseMacroSpecs :: Text -> Maybe MacroSpec
parseMacroSpecs txt =
  case parseOnly (macroSpecs <* endOfInput) txt of
    Left{}     -> Nothing
    Right spec -> Just spec

macroSpecs :: Parser MacroSpec
macroSpecs =
  cons <$> P.takeWhile1 isAlpha
       <*> optional (char '?')
       <*  P.skipSpace
       <*> macroSpecs
    <|> pure (MacroSpec NoArg)
 where
 cons desc (Just _) (MacroSpec rest) = MacroSpec (OptTokenArg (Text.unpack desc) rest)
 cons desc Nothing  (MacroSpec rest) = MacroSpec (ReqTokenArg (Text.unpack desc) rest)

-- | Parse a 'Text' searching for the expansions as specified in
-- 'ExpansionChunk'. @$$@ is used to escape a single @$@.
parseExpansion :: Text -> Maybe [ExpansionChunk]
parseExpansion txt =
  case parseOnly (many parseChunk <* endOfInput) txt of
    Left{}       -> Nothing
    Right chunks -> Just chunks

parseChunk :: Parser ExpansionChunk
parseChunk =
  choice
    [ LiteralChunk     <$> P.takeWhile1 (/= '$')
    , LiteralChunk "$" <$  P.string "$$"
    , string "${" *> parseDefaulted <* char '}'
    , char '$' *> parseVariable
    ]

parseDefaulted :: Parser ExpansionChunk
parseDefaulted =
  construct
    <$> parseVariable
    <*> optional (char '|' *> P.takeWhile1 (/= '}'))
 where
 construct ch Nothing  = ch
 construct ch (Just l) = DefaultChunk ch l

parseVariable :: Parser ExpansionChunk
parseVariable = IntegerChunk  <$> P.decimal
            <|> VariableChunk <$> P.takeWhile1 isAlpha

-- | Attempt to expand all of the elements in the given list using
-- the two expansion functions. If the expansion of any chunk
-- fails the whole expansion fails.
resolveMacroExpansions ::
  (Text    -> Maybe Text) {- ^ variable resolution       -} ->
  (Integer -> Maybe Text) {- ^ argument index resolution -} ->
  [ExpansionChunk]        {- ^ chunks                    -} ->
  Maybe Text              {- ^ concatenated, expanded chunks -}
resolveMacroExpansions var arg xs = Text.concat <$> traverse resolve1 xs
  where
    resolve1 (LiteralChunk lit) = Just lit
    resolve1 (VariableChunk v)  = var v
    resolve1 (IntegerChunk i)   = arg i
    resolve1 (DefaultChunk p d) = resolve1 p <|> Just d

presolveMacroExpansions
  :: Text
  -> ArgumentSpec t
  -> [ExpansionChunk]
  -> Either Text (MacroCommand t)
presolveMacroExpansions mname spec
  = fmap ((Text.concat .) . sequence) . traverse (presolveChunk mname resolver)
 where
 resolver = makeArgResolver spec id

presolveChunk
  :: Text
  -> ArgResolver t
  -> ExpansionChunk
  -> Either Text (MacroCommand t)
presolveChunk mname resolver chunk
  = case chk of
      LiteralChunk txt -> Right $ pure txt
      VariableChunk v
        | Just res <- varResolver v
        -> handleDefault mname v res def
        | otherwise
        -> Left $ "macro '" <> mname
        <> "' used unknown variable '" <> v <> "'"
      IntegerChunk n
        | Just res <- resolver (Just n)
        -> handleDefault mname (Text.pack $ show n)
             (promapResolved arguments Text.pack res) def
        | otherwise
        -> Left $ "macro '" <> mname
        <> "' used undeclared argument " <> Text.pack (show n)
      DefaultChunk _ _ -> Left $ "internal error: macro chunk invariant failed"
 where
 def | DefaultChunk _ def <- chunk = Just def
     | otherwise                   = Nothing
 chk | DefaultChunk c _ <- chunk = c
     | otherwise                 = chunk

dec :: Maybe Integer -> Maybe Integer
dec = fmap $ subtract 1

-- A type that represents a resolved variable, which may either require a
-- default or not, in the context of an environment 't'.
type Resolved t str = Either (t -> str) (t -> Maybe str)

-- Represents a way of resolving variables of type v.
type Resolver v t str = v -> Maybe (Resolved t str)

-- An 'ArgResolver t' can be built from an 'ArgumentSpec t' to resolve arguments
-- in an environment 't'.
type ArgResolver t = Resolver (Maybe Integer) t String

type ArgResolver' t
   = Maybe Integer
  -> Maybe (t -> Maybe String)

-- Builds an 'ArgResolver t' from an 'ArgumentSpec s' and a function that
-- narrows a 't' to an 's'.
makeArgResolver
  :: ArgumentSpec s
  -> (t -> s)
  -> ArgResolver t
makeArgResolver (ReqTokenArg _ rest) view n
  | Just 0 <- n = Just (Left $ fst . view)
  | otherwise   = makeArgResolver rest (snd . view) (dec n)
makeArgResolver (OptTokenArg _ rest) view n
  | Just 0 <- n = Just (Right $ fmap fst . view)
  | otherwise = Right <$> makeArgResolver' rest (fmap snd . view) (dec n)
makeArgResolver (RemainingArg _)     view n
  | Nothing <- n = Just (Left view)
  | otherwise    = Nothing
makeArgResolver NoArg _ _ = Nothing

-- As 'makeArgResolver', but in a context where an optional argument has already
-- occurred, and thus all subsequent arguments may not exist.
makeArgResolver'
  :: ArgumentSpec s
  -> (t -> Maybe s)
  -> ArgResolver' t
makeArgResolver' (ReqTokenArg _ rest) view n
  | Just 0 <- n = Just (fmap fst . view)
  | otherwise   = makeArgResolver' rest (fmap snd . view) (dec n)
makeArgResolver' (OptTokenArg _ rest) view n
  | Just 0 <- n = Just (fmap fst . view')
  | otherwise   = makeArgResolver' rest (fmap snd . view') (dec n)
 where view' = join . view
makeArgResolver' (RemainingArg _)      view n
  | Nothing <- n = Just view
  | otherwise    = Nothing
makeArgResolver' NoArg _ _ = Nothing

-- Resolves the pre-defined variables in a 'MacroEnv'.
varResolver :: Resolver Text (MacroEnv t) Text
varResolver v
  | v == "network"    = Just (Right network)
  | v == "channel"    = Just (Right channel)
  | v == "nick"       = Just (Right nick)
  | v == "disconnect" = Just (Right disconnect)
  | otherwise         = Nothing

-- Handles the defaulting cases for well-scoped macros. It is illegal to give a
-- default for a reference that can't be undefined, and to fail to give a
-- default for one that may be undefined.
handleDefault
  :: Text -- ^ macro name
  -> Text -- ^ variable name
  -> Resolved (MacroEnv t) Text
  -> Maybe Text -- ^ default value
  -> Either Text (MacroCommand t)
handleDefault mname vname res def =
  case (res, def) of
    (Left noDefault, Nothing) -> Right noDefault
    (Right yesDefault, Just d) -> Right $ fromMaybe d . yesDefault
    (Left _, Just _) -> Left $ redundantDefaultMsg mname vname
    (Right _, Nothing) -> Left $ missedDefaultMsg mname vname

promapResolved
  :: (t' -> t) -> (str -> str')
  -> Either (t -> str) (t -> Maybe str)
  -> Either (t' -> str') (t' -> Maybe str')
promapResolved tf sf
  = bimap (\g -> sf . g . tf) (\h -> fmap sf . h . tf)

redundantDefaultMsg :: Text -> Text -> Text
redundantDefaultMsg mname vname
  = "macro '" <> mname
 <> "' gave '" <> vname
 <> "' a default value even though it will never be undefined"

missedDefaultMsg :: Text -> Text -> Text
missedDefaultMsg mname vname
  = "macro '"
 <> mname <> "' failed to give a default for '"
 <> vname <> "' even though it may be undefined."
