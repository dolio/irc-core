{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.Digraphs
Description : Character mnemonics
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a view of the built-in digraph list.

-}
module Client.View.Digraphs (digraphLines) where

import           Client.Image.Message (cleanChar)
import           Client.Image.PackedImage
import           Client.State
import           Data.List
import           Data.List.Split
import qualified Data.Text as Text
import           Digraphs
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image (wcwidth, wcswidth)

-- | Render the lines of a table showing all of the available digraph entries
digraphLines ::
  Int         {- ^ draw width   -} ->
  ClientState {- ^ client state -} ->
  [Image']    {- ^ output lines -}
digraphLines w st
  = map (mconcat . intersperse sep)
  $ chunksOf entriesPerLine
  $ map (text' defAttr)
  $ matcher
  $ map (Text.pack . drawEntry)
  $ digraphListToList digraphs
  where
    matcher        = maybe id filter (clientMatcher st)
    entriesPerLine = max 1 -- just in case?
                   $ (w + sepWidth) `quot` (entryWidth + sepWidth)

entryWidth :: Int
entryWidth = 5 -- "Ka カ"

sepWidth :: Int
sepWidth = imageWidth sep

sep :: Image'
sep = text' defAttr "   "

drawEntry :: (Char,Char,Char) -> String
drawEntry (x,y,z) = output ++ replicate (entryWidth - wcswidth output) ' '
  where
    output = x:y:z2
    dottedCircle = '\x25cc'
    z1 = cleanChar z
    z2 | wcwidth z1 == 0 = [' ', dottedCircle, z1]
       | otherwise       = [' ', z1]
