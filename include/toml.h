#define INSTANCES (Data, Eq, Ord, Read, Show, Typeable)
#define PRISM(NAME,PARENT,CHILD) \
    _/**/NAME :: Prism' PARENT CHILD; \
    _/**/NAME = prism' NAME $ \ x -> case x of \
                                         NAME m -> Just m; \
                                         _ -> Nothing
