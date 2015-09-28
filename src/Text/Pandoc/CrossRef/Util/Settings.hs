{-# LANGUAGE CPP #-}
module Text.Pandoc.CrossRef.Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc
import Text.Pandoc.Builder
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Gen
import Text.Pandoc.CrossRef.Util.Meta

getSettings :: Meta -> IO Meta
getSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" (meta <> defaultMeta))
#if MIN_VERSION_pandoc(1,14,0)
  let Pandoc dtve _ = either (error . show) id $ readMarkdown def ("---\n" ++ yaml ++ "\n---")
#else
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
#endif
  return $ meta <> dtve <> defaultMeta

defaultMeta :: Meta
defaultMeta =
     figureTitle (str "Figure")
  <> supFigureTitle (str "Figure" <> space <> str "S")
  <> tableTitle (str "Table")
  <> listingTitle (str "Listing")
  <> titleDelim (str ":")
  <> chapDelim (str ".")
  <> rangeDelim (str "-")
  <> figPrefix ([str "fig.", str "figs."])
  <> supFigPrefix ([str "fig." <> space <> str "S", str "figs." <> space <> str "S"])
  <> eqnPrefix ([str "eq." , str "eqns."])
  <> tblPrefix ([str "tbl.", str "tbls."])
  <> lstPrefix ([str "lst.", str "lsts."])
  <> secPrefix ([str "sec.", str "secs."])
  <> lofTitle (header 1 $ text "List of Figures")
  <> lotTitle (header 1 $ text "List of Tables")
  <> lolTitle (header 1 $ text "List of Listings")
  <> figureTemplate (var "figureTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> supFigureTemplate (var "supFigureTitle" <> var "i" <> var "titleDelim" <> space <> var "t")
  <> tableTemplate (var "tableTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> listingTemplate (var "listingTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> crossrefYaml (MetaString "pandoc-crossref.yaml")
  <> chaptersDepth (MetaString "1")
  where var = displayMath
