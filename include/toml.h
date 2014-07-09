#define INSTANCES (Data, Eq, Ord, Read, Show, Typeable)
#define PrismP Prism'
#define PRISM(N,P,C) _/**/N :: PrismP P C; _/**/N = prism' N $ \ x -> case x of N m -> Just m; _ -> Nothing
