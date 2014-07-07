{-# LANGUAGE TemplateHaskell #-}

module Text.Toml.TH (prismatic) where

import Control.Lens
import Language.Haskell.TH

prismatic :: String -> String -> String -> Q [Dec]
prismatic name parent child = do
    let prismName = mkName $ '_' : name
        conName = mkName name
    conArg <- newName "a"
    let ty = toType child
    x <- newName "x"
    let xpat = varP x
    -- 7.6 doesn't support pattern splices
    f <- funD prismName [clause []
        (normalB [|prism' $(conE conName) $(lamCaseE
                [match (conP conName [varP conArg])
                    (normalB [|Just $(varE conArg)|])
                    []
                , match wildP (normalB [|Nothing|]) []])|])
        []]
    t <- [t|Prism' $(conT $ mkName parent) $(ty)|]
    return [SigD prismName t, f]
    where
        toType = foldl1 appT . map (conT . mkName) . words
