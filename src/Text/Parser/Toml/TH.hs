{-# LANGUAGE TemplateHaskell #-}

module Text.Parser.Toml.TH where

import Control.Lens
import Language.Haskell.TH

prismatic :: String -> String -> String -> Q [Dec]
prismatic name parent child = do
    let prismName = mkName $ '_' : name
        conName = mkName name
    conArg <- newName "a"
    let ty = toType child
    f <- funD prismName [clause []
        (normalB [|prism' $(conE conName) (\ x -> case x of
                    $(conP conName [varP conArg]) -> Just $(varE conArg)
                    _ -> Nothing)|])
        []]
    t <- [t|Prism' $(conT $ mkName parent) $(ty)|]
    return [SigD prismName t, f]
    where
        toType = foldl1 appT . map (conT . mkName) . words
