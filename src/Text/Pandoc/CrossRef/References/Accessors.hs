module Text.Pandoc.CrossRef.References.Accessors where

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Accessor

imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})

lstRefs' :: Accessor References RefMap
lstRefs' new r@References{lstRefs=old} = (old, r{lstRefs=new})

secRefs' :: Accessor References RefMap
secRefs' new r@References{secRefs=old} = (old, r{secRefs=new})

supFigRefs' :: Accessor References RefMap
supFigRefs' new r@References{supFigRefs=old} = (old, r{supFigRefs=new})