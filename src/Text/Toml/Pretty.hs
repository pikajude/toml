-- | Pretty-print TOML.
module Text.Toml.Pretty (ppr) where

import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.ISO8601
import Text.Toml
import Text.PrettyPrint.ANSI.Leijen

-- | Pretty-prints TOML as JSON.
--
-- Pretty printing this example document:
--
-- @
-- # This is a TOML document. Boom.
--
-- title = "TOML Example"
--
-- [owner]
-- name = "Tom Preston-Werner"
-- organization = \"GitHub\"
-- bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
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
-- yields the JSON:
--
-- @
-- {
--   "clients": {
--     "data": [["gamma", "delta"], [1, 2]],
--     "hosts": ["alpha", "omega"]
--   },
--   "database": {
--     "connection_max": 5000,
--     "enabled": true,
--     "ports": [8001, 8001, 8002],
--     "server": "192.168.1.1"
--   },
--   "owner": {
--     "bio": "GitHub Cofounder & CEO\\nLikes tater tots and beer.",
--     "dob": "1979-05-27T07:32:00Z",
--     "name": "Tom Preston-Werner",
--     "organization": \"GitHub\"
--   },
--   "servers": [
--     {
--       "dc": "eqdc10",
--       "ip": "10.0.0.1",
--       "name": "alpha"
--     },
--     {
--       "dc": "eqdc10",
--       "ip": "10.0.0.2",
--       "name": "beta"
--     }
--   ],
--   "title": "TOML Example"
-- }
-- @
--
-- (as a consequence of the internal structure of 'Map', the keys will
-- always be in alphabetical order.)
ppr :: Toml -> Doc
ppr t = vsep
      [ lbrace
      , indent 2 (vcat (punctuate comma (map pprPair $ M.toList t)))
      , rbrace ]

pprPair :: (Text, Value) -> Doc
pprPair (k,v) = pprScalar (String k) <> text ":" <+> pprValue v

pprValue :: Value -> Doc
pprValue (Table t) = ppr t
pprValue (Tables ts) = vsep
    [ lbracket
    , indent 2 (vcat (punctuate comma $ map ppr $ toList ts))
    , rbracket ]
pprValue (Scalar s) = pprScalar s

pprScalar :: Scalar -> Doc
pprScalar (String t) = text (B.toString . encode $ toJSON t)
pprScalar (Decimal d) = integer d
pprScalar (Floating f) = double f
pprScalar (Bool True) = text "true"
pprScalar (Bool False) = text "false"
pprScalar (Date d) = text $ show (formatISO8601 d)
pprScalar (List vs) = brackets $ hsep
    (punctuate comma . map pprScalar $ toList vs)
