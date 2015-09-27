{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.CrossRef.Util.Options (Options(..), getOptions) where
import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util (capitalizeFirst)
import Text.Pandoc.Walk
import Data.Default
-- import Control.Monad.Identity

data Options = Options { useCleveref :: Bool
                       , chapDepth   :: Int
                       , useListings :: Bool
                       , cbCaptions  :: Bool
                       , autoSecLab  :: Bool
                       , figPrefix   :: Bool -> Int -> [Inline]
                       , supFigPrefix :: Bool -> Int -> [Inline]
                       , eqnPrefix   :: Bool -> Int -> [Inline]
                       , tblPrefix   :: Bool -> Int -> [Inline]
                       , supTblPrefix   :: Bool -> Int -> [Inline]
                       , lstPrefix   :: Bool -> Int -> [Inline]
                       , secPrefix   :: Bool -> Int -> [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , lolTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: Template
                       , supFigureTemplate :: Template
                       , tableTemplate  :: Template
                       , supTableTemplate  :: Template
                       , listingTemplate :: Template
                       }

getOptions :: Meta -> Maybe Format -> Options
getOptions dtv fmt =
  Options {
      useCleveref = getMetaBool "cref" dtv
    , chapDepth   = if getMetaBool "chapters" dtv
        then read $ getMetaString "chaptersDepth" dtv
        else 0
    , useListings = getMetaBool "listings" dtv
    , cbCaptions  = getMetaBool "codeBlockCaptions" dtv
    , autoSecLab  = getMetaBool "autoSectionLabels" dtv
    , figPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "figPrefix"
    , supFigPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "supFigPrefix"
    , eqnPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "eqnPrefix"
    , tblPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "tblPrefix"
    , supTblPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "supTblPrefix"
    , lstPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "lstPrefix"
    , secPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "secPrefix"
    , chapDelim   = getMetaInlines "chapDelim" dtv
    , rangeDelim  = getMetaInlines "rangeDelim" dtv
    , lofTitle    = getMetaBlock "lofTitle" dtv
    , lotTitle    = getMetaBlock "lotTitle" dtv
    , lolTitle    = getMetaBlock "lolTitle" dtv
    , outFormat   = fmt
    , figureTemplate = makeTemplate dtv $ getMetaInlines "figureTemplate" dtv
    , supFigureTemplate = makeTemplate dtv $ getMetaInlines "supFigureTemplate" dtv
    , tableTemplate  = makeTemplate dtv $ getMetaInlines "tableTemplate" dtv
    , supTableTemplate  = makeTemplate dtv $ getMetaInlines "supTableTemplate" dtv
    , listingTemplate = makeTemplate dtv $ getMetaInlines "listingTemplate" dtv
  }

tryCapitalizeM :: (Functor m, Monad m, Walkable Inline a, Default a, Eq a) =>
        (String -> m a) -> String -> Bool -> m a
tryCapitalizeM f varname capitalize
  | capitalize = do
    res <- f (capitalizeFirst varname)
    case res of
      xs | xs == def -> f varname >>= walkM capStrFst
         | otherwise -> return xs
  | otherwise  = f varname
  where
    capStrFst (Str s) = return $ Str $ capitalizeFirst s
    capStrFst x = return x

-- tryCapitalize :: (Walkable Inline a, Default a, Eq a) =>
--         (String -> a) -> String -> Bool -> a
-- tryCapitalize = ((runIdentity .) .) . tryCapitalizeM . (return .)
