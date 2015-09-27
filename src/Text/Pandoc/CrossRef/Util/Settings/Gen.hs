{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc
import qualified Data.Map as M

import Text.Pandoc.CrossRef.Util.Settings.Template
import Text.Pandoc.Builder

$(concat `fmap` mapM genSetting
  [ "figureTitle"
  , "supFigureTitle"
  , "tableTitle"
  , "supTableTitle"
  , "listingTitle"
  , "titleDelim"
  , "chapDelim"
  , "rangeDelim"
  , "figPrefix"
  , "supFigPrefix"
  , "eqnPrefix"
  , "tblPrefix"
  , "supTblPrefix"
  , "lstPrefix"
  , "secPrefix"
  , "lofTitle"
  , "lotTitle"
  , "lolTitle"
  , "figureTemplate"
  , "supFigureTemplate"
  , "tableTemplate"
  , "supTableTemplate"
  , "listingTemplate"
  , "crossrefYaml"
  , "chaptersDepth"
  ])
