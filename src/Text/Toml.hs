{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Toml (
    -- * Parsing TOML
    -- *** The parser
    toml,

    -- *** The TOML AST
    Toml, Value (..), Scalar (..),

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
import Data.Char
import Data.Data
import Data.Foldable (toList)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric
import Text.Toml.TH
import Text.Trifecta hiding (decimal, string)
import qualified Text.Trifecta as X

#define INSTANCES (Data, Eq, Ord, Read, Show, Typeable)

-- $setup
-- The code examples in this module require GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings

-- | Represents TOML.
type Toml = Map Text Value

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
           | Tables (Seq Toml)
           | Scalar Scalar
           deriving INSTANCES

instance ToJSON Value where
    toJSON (Table t) = toJSON t
    toJSON (Tables ts) = toJSON . toList $ ts
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
prismatic "Tables" "Value" "Seq Toml"
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

instance MarkParsing d m => MarkParsing d (Unlined m) where
    mark = Unlined mark
    release = Unlined . release


type KeyPath = [Text]

-- not exposed, used during parsing
data Directive = SetD String Scalar
               | TableD TableType KeyPath
               deriving Show

data TableType = Dictionary | Array deriving (Eq, Show)

data ParseState = ParseState
                { _definedTables :: [(TableType, KeyPath)]
                , _definedKeypaths :: [KeyPath]
                }

makeLenses ''ParseState

-- | Parse some TOML.
toml :: (TokenParsing m, MonadPlus m) => m Toml
toml = fmap generate . runUnlined . (`evalStateT` ParseState [] [])
     . fmap catMaybes
     $ (whiteSpace *> entity) `sepBy` char '\n' <* eof where
    entity = anyOf $
                [ comment
                , commented tablename
                , commented define
                ] ++ [Nothing <$ whiteSpace]
    commented = (<* optional comment)
    comment = Nothing <$ (char '#' *> many (satisfy (/= '\n'))) <?> "comment"
    logSet k = do
        parents <- use definedTables
        let path' = case parents of
                [] -> [k]
                ((_, p) : _) -> p ++ [k]
        definedKeypaths %= (path' :)
        checkConflicts
    checkConflicts = do
        k <- use definedKeypaths
        ts <- map snd <$> use definedTables
        case k `intersect` ts of
            [] -> return ()
            (x : _) -> fail $ "conflicting definitions of '"
                           ++ intercalate "." (map T.unpack x)
                           ++ "' (one is an attribute, one is a table)"
    push x = do
        ensureUnique x
        definedTables %= (x :)
        checkConflicts
    ensureUnique (typ, path') = do
        ms <- use definedTables
        case [ (ty, ty == typ) | (ty, ps) <- ms, ps == path' ] of
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
        n <- keyDotless `sepBy1` char '.' <?> "table name"
        _ <- char ']'
        when isArray $ () <$ char ']'
        let path' = map pack n
        push (ty, path')
        return . Just $ TableD ty path'
    define = do
        k <- try (token key) <?> "key name"
        logSet (pack k)
        _ <- symbol "="
        v <- token value
        return . Just $ SetD k v
    key = some (satisfy (isKeyChar True False))
    keyDotless = some (satisfy (isKeyChar False True))
    isKeyChar allowDot allowEq x =
        x `notElem` "[]#" && not (isSpace x)
            && (allowDot || x /= '.') && (allowEq || x /= '=')
    value = anyOf [list', date', number, str, bool']
    list' = (do
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
    bool' = Bool True <$ X.string "true"
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
    date' = (<?> "valid date") $ do
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

instance IsString Toml where
    fromString v = case parseString toml mempty v of
        Success x -> x
        _ -> error "invalid toml doc"

generate :: [Directive] -> Toml
generate ds = snd $ execState (go ds) (Nothing, M.empty) where
    go (SetD k v : xs) = do
        set (pack k) (Scalar v)
        go xs
    go (t@TableD {} : xs) = do
        pushParent t
        _1 .= Just t
        go xs
    go [] = return ()
    pushParent (TableD Dictionary p) = _2 . path p ?= Table M.empty
    pushParent (TableD Array p) = _2 . path p . non (Tables (S.fromList [])) . _Tables
        %= (|> M.empty)
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
path (x : xs) = at x . non (Table M.empty)
              . failing _Table (_Tables . _last) . path xs

-- | Traverse a TOML table, given a key name.
--
-- >>> "[foo]" ^? table "foo"
-- Just (fromList [])
table :: Text -> Traversal' Toml Toml
table t = ix t . _Table

-- | Traverse a list of TOML tables, given their shared key name.
--
-- >>> "[[foo]]\n[[foo]]" & lengthOf (tables "foo")
-- 2
tables :: Text -> Traversal' Toml Toml
tables ts = ix ts . _Tables . traverse

-- | Target a generic 'Scalar' value.
--
-- >>> "foo = \"bar\"" ^? scalar "foo"
-- Just (String "bar")
scalar :: Text -> Traversal' Toml Scalar
scalar t = ix t . _Scalar

-- | Given a key name, target a 'String' value. Will fail if the key is
-- present, but is not a 'String'.
--
-- >>> "foo = \"bar\"" ^? string "foo"
-- Just "bar"
string :: Text -> Traversal' Toml Text
string t = ix t . _Scalar . _String

-- | Given a key name, target a 'Decimal' value. Will fail if the key is
-- present, but is not a 'Decimal'.
--
-- >>> "foo = 1234" ^? decimal "foo"
-- Just 1234
decimal :: Text -> Traversal' Toml Integer
decimal t = ix t . _Scalar . _Decimal

-- | Given a key name, target a 'Floating' value. Will fail if the key is
-- present, but is not 'Floating'.
--
-- >>> "foo = 1234.567" ^? floating "foo"
-- Just 1234.567
floating :: Text -> Traversal' Toml Double
floating t = ix t . _Scalar . _Floating

-- | Given a key name, target a 'Bool' value. Will fail if the key is
-- present, but is not a 'Bool'.
--
-- >>> "foo = false" ^? bool "foo"
-- Just False
bool :: Text -> Traversal' Toml Bool
bool t = ix t . _Scalar . _Bool

-- | Given a key name, target a 'Date' value. Will fail if the key is
-- present, but is not a 'Date'.
--
-- >>> "foo = 1994-04-28T05:30:22Z" ^? date "foo"
-- Just 1994-04-28 05:30:22 UTC
date :: Text -> Traversal' Toml UTCTime
date t = ix t . _Scalar . _Date

-- | Given a key name, target a 'List' value. Will fail if the key is
-- present, but is not a 'List'.
--
-- >>> "foo = [1, 2, 3]" ^? list "foo"
-- Just (fromList [Decimal 1,Decimal 2,Decimal 3])
list :: Text -> Traversal' Toml (Vector Scalar)
list t = ix t . _Scalar . _List

-- | Given a key name and a prism, target all the elements of a 'List' that
-- match the prism. Will fail if the key is present, but is not a 'List'.
-- Will fail if the list elements do not match the prism. (In TOML,
-- lists are homogeneous, so you will always either retrieve the whole list
-- or none of it.)
--
-- >>> "foo = [1, 2, 3]" ^.. listOf "foo" _Decimal
-- [1,2,3]
listOf :: Text -> Prism' Scalar p -> Traversal' Toml p
listOf t p = ix t . _Scalar . _List . traverse . p

-- $lensdoc
-- Traversing anything JSON-like in Haskell is usually a huge pain unless
-- you can conceive some kind of convenient DSL for it. In this case we use
-- utilities provided by the @lens@ package.
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
-- >>> Just ast <- parseFromFile toml "example.txt"
--
-- Using the functions that @lens@ provides and the combinators here, you
-- can then traverse deeply into the resultant structure to retrieve data:
--
-- >>> ast ^? table "owner" . string "bio" -- find Tom's bio
-- Just "GitHub Cofounder & CEO\nLikes tater tots and beer."
--
-- >>> ast ^.. tables "servers" . string "ip" -- list every server IP
-- ["10.0.0.1","10.0.0.2"]
--
-- >>> ast ^.. table "database" . listOf "ports" _Decimal -- retrieve the client hostnames
-- [8001,8001,8002]
--
-- You can even modify the AST:
--
-- >>> let newHosts = ast & table "clients" . listOf "hosts" _String %~ T.reverse
-- >>> newHosts ^? table "clients" . list "hosts"
-- Just (fromList [String "ahpla",String "agemo"])
