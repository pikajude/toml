{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parser.Toml (
    -- * Parsing TOML
    -- *** The parser
    toml,

    -- *** The TOML AST
    Toml (..), Value (..), Scalar (..),

    -- * Lenses for TOML
    -- $lensdoc

    -- *** Top-level value lenses
    table, tables, scalar,

    -- *** Scalar lenses
    string, decimal, floating, bool, date, list, listOf,

    -- *** Top-level value prisms
    _Table, _Tables, _Scalar,

    -- *** Scalar prisms
    _String, _Decimal, _Floating, _Bool, _Date, _List
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding (anyOf, inside, set)
import Control.Monad.State
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.Char
import Data.Data
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric
import Text.Parser.Toml.TH
import Text.Trifecta hiding (decimal, string)
import qualified Text.Trifecta as X

#define INSTANCES (Data, Eq, Ord, Read, Show, Typeable)

-- | Represents TOML.
newtype Toml = Toml { unToml :: Map Text Value }
             deriving INSTANCES

instance ToJSON Toml where
    toJSON (Toml xs) = A.object . map (\ (a, b) -> a A..= toJSON b) $ M.toList xs

instance NFData Toml where
    rnf (Toml t) = rnf t

-- | Top-level TOML values.
--
-- @
-- # dictionary
-- [some_dictionary]
-- val1 = "foo"
--
-- # list of dictionaries
-- [[some_item]]
-- val2 = "bar"
--
-- [[some_item]]
-- val3 = "baz"
-- @
data Value = Table Toml
           | Tables (Vector Toml)
           | Scalar Scalar
           deriving INSTANCES

instance ToJSON Value where
    toJSON (Table t) = toJSON t
    toJSON (Tables ts) = toJSON ts
    toJSON (Scalar s) = toJSON s

instance NFData Value where
    rnf (Table t) = rnf t
    rnf (Tables ts) = rnf ts
    rnf (Scalar s) = rnf s

-- | TOML scalar values; i.e., values that can be associated with keys.
--
-- @
-- str = "bar"
-- decimal = 1
-- floating = 1.0
-- date = 1999-12-31T23:59:59Z
-- boolean = true
-- list = [ 1, 2, 3, 4, 5 ]
-- @
data Scalar = String Text
            | Decimal Integer
            | Floating Double
            | Bool Bool
            | Date UTCTime
            | List (Vector Scalar)
            deriving INSTANCES

instance ToJSON Scalar where
    toJSON (String t) = toJSON t
    toJSON (Decimal i) = toJSON i
    toJSON (Floating d) = toJSON d
    toJSON (Bool b) = toJSON b
    toJSON (Date u) = toJSON $ formatISO8601 u
    toJSON (List vs) = toJSON $ V.toList vs

instance NFData Scalar where
    rnf (String t) = rnf t
    rnf (Decimal t) = rnf t
    rnf (Floating t) = rnf t
    rnf (Bool t) = rnf t
    rnf (Date t) = rnf t
    rnf (List t) = rnf t

prismatic "Table" "Value" "Toml"
prismatic "Tables" "Value" "Vector Toml"
prismatic "Scalar" "Value" "Scalar"

prismatic "String" "Scalar" "Text"
prismatic "Decimal" "Scalar" "Integer"
prismatic "Floating" "Scalar" "Double"
prismatic "Bool" "Scalar" "Bool"
prismatic "Date" "Scalar" "UTCTime"
prismatic "List" "Scalar" "Vector Scalar"


-- this is an orphan
instance DeltaParsing p => DeltaParsing (Unlined p) where
    line = lift line
    position = lift position
    slicedWith f = Unlined . slicedWith f . runUnlined


-- not exposed, used during parsing
data Directive = SetD String Scalar
               | TableD TableType [Text]
               deriving Show

data TableType = Dictionary | Array deriving (Eq, Show)

-- | Parse some TOML.
toml :: (TokenParsing m, MonadPlus m) => m Toml
toml = fmap generate . runUnlined . (`evalStateT` []) . fmap catMaybes
     $ (whiteSpace *> entity) `sepBy` char '\n' <* eof where
    entity = anyOf $
                [ comment
                , commented tablename
                , commented define
                ] ++ [Nothing <$ whiteSpace]
    commented = (<* optional comment)
    comment = Nothing <$ (char '#' *> many (satisfy (/= '\n'))) <?> "comment"
    push x@(TableD ty path') = do
        ensureUnique (ty, path')
        modify (x:)
    push x = modify (x:)
    ensureUnique (typ, path') = do
        ms <- get
        case [ (ty, ty == typ) | TableD ty ps <- ms, ps == path' ] of
            ((_, False) : _) -> fail $ "redefinition of table '"
                     ++ intercalate "." (map T.unpack path')
                     ++ "' with different type"
            ((Dictionary, _) : _) -> fail $ "redefinition of table '"
                     ++ intercalate "." (map T.unpack path')
                     ++ "'"
            _ -> return ()
    tablename = do
        _ <- char '['
        m <- optional (char '[')
        let isArray = isJust m
            ty = if isArray then Array else Dictionary
        n <- key `sepBy1` char '.' <?> "table name"
        _ <- char ']'
        when isArray $ () <$ char ']'
        let path' = map pack n
        push (TableD ty path')
        return . Just $ TableD ty path'
    define = do
        k <- try (token key) <?> "key name"
        _ <- symbol "="
        v <- token value
        push (SetD k v)
        return . Just $ SetD k v
    key = (:) <$> letter <*> many (alphaNum <|> char '_')
    value = anyOf [list, date, number, str, bool]
    list = (do
        values <- brackets (commaSep' value)
        needHomogeneous values
        return . List $ V.fromList values) <?> "list"
    needHomogeneous values = when (length (filter ((> 0) . length) [
        [ () | String _ <- values ]
      , [ () | Decimal _ <- values ]
      , [ () | Floating _ <- values ]
      , [ () | Bool _ <- values ]
      , [ () | Date _ <- values ]
      , [ () | List _ <- values ]]) > 1)
        $ fail "mixed datatypes in list"
    bool = Bool True  <$ X.string "true"
       <|> Bool False <$ X.string "false"
       <?> "boolean"
    commaSep' m = (<|> token' (pure [])) $ try $
        (:) <$> token' m <*> many (do
            _ <- token' (char ',')
            token' m)
    token' m = many (satisfy isSpace) *> m <* many (satisfy isSpace)
    str = (<?> "string") $ do
        _ <- char '"'
        inside <- concat <$> many validSequence
        _ <- char '"' <?> "closing quote"
        return . String $ pack inside
    number = (either Decimal Floating <$> integerOrDouble) <?> "number"
    anyOf = foldr1 (<|>)
    backslashed = do
        _ <- char '\\'
        c <- anyChar
        case c of
            'b' -> return "\b"
            't' -> return "\t"
            'n' -> return "\n"
            'f' -> return "\f"
            'r' -> return "\r"
            '"' -> return "\""
            '/' -> return "/"
            '\\' -> return "\\"
            x -> fail $ "unknown escape sequence '\\" ++ [x] ++ "'"
    validSequence = fmap return
        (satisfy (\ x -> x > '\US' && x `notElem` "\"\\") <?> "character")
                <|> ((backslashed <|> unicodeEscape) <?> "escape sequence")
    unicodeEscape = do
        _ <- X.string "\\u"
        point <- count 4 hexDigit
        unless (all (liftM2 (||) isUpper isDigit) point) $
            panic point
        case readHex point of
            [(num, "")] -> return [chr $ fromIntegral (num :: Integer)]
            _ -> panic point
        where
            panic p = fail $ "can't parse Unicode sequence '" ++ p ++ "'"
    date = (<?> "valid date") $ do
        d <- fmap concat . try $ sequence
          [ count 4 digit
          , X.string "-"
          , count 2 digit
          , X.string "-"
          , count 2 digit
          , X.string "T"
          , count 2 digit
          , X.string ":"
          , count 2 digit
          , X.string ":"
          , count 2 digit
          , X.string "Z" ]
        case parseISO8601 d of
            Just d8 | formatISO8601 d8 == d -> return $ Date d8
            _ -> fail $ "invalid date " ++ show d ++ ""

generate :: [Directive] -> Toml
generate ds = snd $ execState (go ds) (Nothing, Toml M.empty) where
    go (SetD k v : xs) = do
        set (pack k) (Scalar v)
        go xs
    go (t@TableD{} : xs) = do
        pushParent t
        _1 .= Just t
        go xs
    go [] = return ()
    pushParent (TableD Dictionary p) = _2 . path p ?= Table (Toml M.empty)
    pushParent (TableD Array p) = _2 . path p . non (Tables (V.fromList [])) . _Tables
        %= (|> Toml M.empty)
    pushParent x = error (show x)

set :: Text -> Value -> State (Maybe Directive, Toml) ()
set k v = do
    parent <- use _1
    case parent of
        Just (TableD Dictionary p) ->
            _2 . path p . _Just . _Table . at k ?= v
        Just (TableD Array p) ->
            _2 . path p . _Just . _Tables . _last
               . at k ?= v
        _ -> _2 . at k ?= v

path :: Applicative f
     => [Text] -> (Maybe Value -> f (Maybe Value)) -> Toml -> f Toml
path [] = error "path"
path [x] = at x
path (x : xs) = at x . non (Table (Toml M.empty))
              . failing _Table (_Tables . _last) . path xs

type instance IxValue Toml = Value
type instance Index Toml = Text

instance Ixed Toml where
    ix k f t = case M.lookup k (unToml t) of
        Just v -> f v <&> \ v' -> Toml (M.insert k v' (unToml t))
        Nothing -> pure t

instance At Toml where
    at k f t = f mv <&> \ r -> case r of
        Nothing -> maybe t (const (Toml (M.delete k (unToml t)))) mv
        Just v' -> Toml $ M.insert k v' (unToml t)
        where mv = M.lookup k (unToml t)

-- | Traverse a TOML table, given a key name.
--
-- @[some_table]@
table :: (Ixed m, IxValue m ~ Value)
      => Index m -> Traversal' m (Map Text Value)
table t = ix t . _Table . iso unToml Toml

-- | Traverse a list of TOML tables, given their shared key name.
--
-- @[[some_table]]@
tables :: (Ixed m, IxValue m ~ Value)
       => Index m -> Traversal' m (Map Text Value)
tables ts = ix ts . _Tables . traverse . iso unToml Toml

-- | Target a generic 'Scalar' value.
--
-- @foo = "bar"@
scalar :: (Ixed m, IxValue m ~ Value)
       => Index m -> Traversal' m Scalar
scalar t = ix t . _Scalar

-- | Given a key name, target a 'String' value. Will fail if the key is
-- present, but is not a 'String'.
--
-- @foo = "bar"@
--
-- >>> doc ^? string "foo"
-- Just "bar"
string :: (Ixed m, IxValue m ~ Value)
       => Index m -> Traversal' m Text
string t = ix t . _Scalar . _String

-- | Given a key name, target a 'Decimal' value. Will fail if the key is
-- present, but is not a 'Decimal'.
--
-- @foo = 1234@
--
-- >>> doc ^? decimal "foo"
-- Just 1234
decimal :: (Ixed m, IxValue m ~ Value)
        => Index m -> Traversal' m Integer
decimal t = ix t . _Scalar . _Decimal

-- | Given a key name, target a 'Floating' value. Will fail if the key is
-- present, but is not 'Floating'.
--
-- @foo = 1234.567@
--
-- >>> doc ^? floating "foo"
-- Just 1234.567
floating :: (Ixed m, IxValue m ~ Value)
         => Index m -> Traversal' m Double
floating t = ix t . _Scalar . _Floating

-- | Given a key name, target a 'Bool' value. Will fail if the key is
-- present, but is not a 'Bool'.
--
-- @foo = false@
--
-- >>> doc ^? bool "foo"
-- Just False
bool :: (Ixed m, IxValue m ~ Value)
     => Index m -> Traversal' m Bool
bool t = ix t . _Scalar . _Bool

-- | Given a key name, target a 'Date' value. Will fail if the key is
-- present, but is not a 'Date'.
--
-- @foo = 1994-04-28T05:30:22Z@
--
-- >>> doc ^? date "foo"
-- Just 1994-04-28 05:30:22 UTC
date :: (Ixed m, IxValue m ~ Value)
     => Index m -> Traversal' m UTCTime
date t = ix t . _Scalar . _Date

-- | Given a key name, target a 'List' value. Will fail if the key is
-- present, but is not a 'List'.
--
-- @foo = [1, 2, 3]@
--
-- >>> doc ^? list "foo"
-- Just (fromList [Decimal 1,Decimal 2,Decimal 3])
list :: (Ixed m, IxValue m ~ Value)
     => Index m -> Traversal' m (Vector Scalar)
list t = ix t . _Scalar . _List

-- | Given a key name and a prism, target all the elements of a 'List' that
-- match the prism. Will fail if the key is present, but is not a 'List'.
-- Will fail if the list elements do not match the prism. (In TOML,
-- lists are homogeneous, so you will always either retrieve the whole list
-- or none of it.)
--
-- @foo = [1, 2, 3]@
--
-- >>> doc ^.. listOf "foo" _Decimal
-- [1,2,3]
listOf :: (Ixed m, IxValue m ~ Value)
       => Index m -> Prism' Scalar p -> Traversal' m p
listOf t p = ix t . _Scalar . _List . traverse . p

-- $lensdoc
-- Traversing anything JSON-like in Haskell is usually a huge pain unless
-- you can conceive some kind of convenient DSL for it. In this case we use
-- utilities provided by the @lens@ package.
--
-- You will notice that every lens provided here is of the form
--
-- @('Ixed' m, 'IxValue' m ~ 'Value') => 'Index' m -> ...@
--
-- 'Toml' has a trivial 'Ixed' instance; any traversal that can be applied
-- to @'Map' 'Text' 'Value'@ can be applied to 'Toml' as well.
--
-- Here's an example document; it's similar to the
-- <https://github.com/toml-lang/toml#example example on GitHub>,
-- with the exception that the \'servers\' table is now a list. This more
-- easily showcases some of the possibilities with the given lenses:
--
-- @
-- # This is a TOML document. Boom.
--
-- title = "TOML Example"
--
-- [owner]
-- name = "Tom Preston-Werner"
-- organization = \"GitHub\"
-- bio = "GitHub Cofounder & CEO\\nLikes tater tots and beer."
-- dob = 1979-05-27T07:32:00Z # First class dates? Why not?
--
-- [database]
-- server = "192.168.1.1"
-- ports = [ 8001, 8001, 8002 ]
-- connection_max = 5000
-- enabled = true
--
-- [[servers]]
-- name = "alpha"
-- ip = "10.0.0.1"
-- dc = "eqdc10"
--
-- [[servers]]
-- name = "beta"
-- ip = "10.0.0.2"
-- dc = "eqdc10"
--
-- [clients]
-- data = [ ["gamma", "delta"], [1, 2] ]
--
-- # Line breaks are OK when inside arrays
-- hosts = [
--   "alpha",
--   "omega"
-- ]
-- @
--
-- You can easily parse it using 'toml':
--
-- >>> ast <- parseFromFileEx toml "example.txt"
-- a giant, ugly AST goes here...
--
-- Using the functions that @lens@ provides and the combinators here, you
-- can then traverse deeply into the resultant structure to retrieve data:
--
-- >>> ast ^? table "owner" . string "bio" -- find Tom's bio
-- Just "GitHub Cofounder & CEO\nLikes tater tots and beer."
--
-- >>> ast ^.. tables "servers" . string "ip" -- list every server IP
-- ["10.0.0.1", "10.0.0.2"]
--
-- >>> ast ^.. table "clients" . list "hosts" . traverse . _String -- retrieve the client hostnames
-- ["alpha", "omega"]
--
-- You can even modify the AST:
--
-- >>> ast & tables "servers" . string "name" <>~ "-server"
