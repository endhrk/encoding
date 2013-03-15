{-# LANGUAGE FlexibleContexts #-}
module Data.Encoding
    (module Data.Encoding.Exception
    ,module Data.Encoding.ByteSource
    ,module Data.Encoding.ByteSink
    ,Encoding(..)
    ,DynEncoding
    ,recode
    ,encodeString
    ,encodeStringExplicit
    ,decodeString
    ,decodeStringExplicit
    ,encodeLazyByteString
    ,encodeLazyByteStringExplicit
    ,decodeLazyByteString
    ,decodeLazyByteStringExplicit
    ,encodeStrictByteString
    ,encodeStrictByteStringExplicit
    ,decodeStrictByteString
    ,decodeStrictByteStringExplicit
    ,encodingFromString
    ,encodingFromStringExplicit
    )
    where

import Data.Encoding.Base
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Data.Encoding.Exception

import Data.Sequence
import Data.Foldable(toList)
import Data.Char

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Error.Class
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Encoding.ASCII
import Data.Encoding.UTF8
import Data.Encoding.UTF16
import Data.Encoding.UTF32
import Data.Encoding.ISO88591
import Data.Encoding.ISO88592
import Data.Encoding.ISO88593
import Data.Encoding.ISO88594
import Data.Encoding.ISO88595
import Data.Encoding.ISO88596
import Data.Encoding.ISO88597
import Data.Encoding.ISO88598
import Data.Encoding.ISO88599
import Data.Encoding.ISO885910
import Data.Encoding.ISO885911
import Data.Encoding.ISO885913
import Data.Encoding.ISO885914
import Data.Encoding.ISO885915
import Data.Encoding.ISO885916
import Data.Encoding.CP1250
import Data.Encoding.CP1251
import Data.Encoding.CP1252
import Data.Encoding.CP1253
import Data.Encoding.CP1254
import Data.Encoding.CP1255
import Data.Encoding.CP1256
import Data.Encoding.CP1257
import Data.Encoding.CP1258
import Data.Encoding.KOI8R
import Data.Encoding.KOI8U
import Data.Encoding.GB18030
import Data.Encoding.MacOSRoman
import Data.Encoding.JISX0201
import Data.Encoding.JISX0208
import Data.Encoding.ISO2022JP
import Data.Encoding.SJIS
import Data.Encoding.CP437
import Data.Encoding.CP737
import Data.Encoding.CP775
import Data.Encoding.CP850
import Data.Encoding.CP852
import Data.Encoding.CP855
import Data.Encoding.CP857
import Data.Encoding.CP860
import Data.Encoding.CP861
import Data.Encoding.CP862
import Data.Encoding.CP863
import Data.Encoding.CP864
import Data.Encoding.CP865
import Data.Encoding.CP866
import Data.Encoding.CP869
import Data.Encoding.CP874
import Data.Char
import Text.Regex

recode :: (Encoding enc1,Encoding enc2,ByteSource m,ByteSink m) => enc1 -> enc2 -> m ()
recode e1 e2 = untilM_ sourceEmpty (decodeChar e1 >>= encodeChar e2)

encodeString :: Encoding enc => enc -> String -> String
encodeString e str = toList $ viewl $ execState (encode e str) empty

encodeStringExplicit :: Encoding enc => enc -> String -> Either EncodingException String
encodeStringExplicit e str = execStateT (encode e str) empty >>= return.toList.viewl

decodeString :: Encoding enc => enc -> String -> String
decodeString e str = evalState (decode e) str

decodeStringExplicit :: Encoding enc => enc -> String -> Either DecodingException String
decodeStringExplicit e str = evalStateT (decode e) str

encodeLazyByteString :: Encoding enc => enc -> String -> LBS.ByteString
encodeLazyByteString e str = runPut $ encode e str

encodeLazyByteStringExplicit :: Encoding enc => enc -> String -> Either EncodingException LBS.ByteString
encodeLazyByteStringExplicit e str = let PutME g = encode e str
                                     in case g of
                                          Left err -> Left err
                                          Right (p,()) -> Right $ runPut p

decodeLazyByteString :: Encoding enc => enc -> LBS.ByteString -> String
decodeLazyByteString e str = runGet (decode e) str

decodeLazyByteStringExplicit :: Encoding enc => enc -> LBS.ByteString -> Either DecodingException String
decodeLazyByteStringExplicit e str = evalStateT (decode e) str

encodeStrictByteString :: Encoding enc => enc -> String -> BS.ByteString
encodeStrictByteString e str = snd $ createStrict $ encode e str

encodeStrictByteStringExplicit :: Encoding enc => enc -> String -> Either EncodingException BS.ByteString
encodeStrictByteStringExplicit e str = let StrictSinkE g = encode e str
                                           (r,bstr) = createStrict g
                                       in case r of
                                            Left err -> Left err
                                            Right _  -> Right bstr

decodeStrictByteString :: Encoding enc => enc -> BS.ByteString -> String
decodeStrictByteString e str = evalState (decode e) str

decodeStrictByteStringExplicit :: Encoding enc => enc -> BS.ByteString -> Either DecodingException String
decodeStrictByteStringExplicit e str = evalStateT (decode e) str

-- | Like 'encodingFromString' but returns 'Nothing' instead of throwing an error
encodingFromStringExplicit :: String -> Maybe DynEncoding
encodingFromStringExplicit codeName = case (normalizeEncoding codeName) of
    -- ASCII
    "ascii"              -> Just $ DynEncoding ASCII
    "646"                -> Just $ DynEncoding ASCII
    "ansi_x3_4_1968"     -> Just $ DynEncoding ASCII
    "ansi_x3.4_1986"     -> Just $ DynEncoding ASCII
    "cp367"              -> Just $ DynEncoding ASCII
    "csascii"            -> Just $ DynEncoding ASCII
    "ibm367"             -> Just $ DynEncoding ASCII
    "iso646_us"          -> Just $ DynEncoding ASCII
    "iso_646.irv_1991"   -> Just $ DynEncoding ASCII
    "iso_ir_6"           -> Just $ DynEncoding ASCII
    "us"                 -> Just $ DynEncoding ASCII
    "us_ascii"           -> Just $ DynEncoding ASCII
    -- UTF-8
    "utf_8"              -> Just $ DynEncoding UTF8
    "u8"                 -> Just $ DynEncoding UTF8
    "utf"                -> Just $ DynEncoding UTF8
    "utf8"               -> Just $ DynEncoding UTF8
    "utf8_ucs2"          -> Just $ DynEncoding UTF8
    "utf8_ucs4"          -> Just $ DynEncoding UTF8
    -- UTF-16
    "utf_16"             -> Just $ DynEncoding UTF16
    "u16"                -> Just $ DynEncoding UTF16
    "utf16"              -> Just $ DynEncoding UTF16
    -- UTF-32
    "utf_32"             -> Just $ DynEncoding UTF32
    -- KOI8-R
    "koi8_r"             -> Just $ DynEncoding KOI8R
    "cskoi8r"            -> Just $ DynEncoding KOI8R
    -- KOI8-I
    "koi8_u"             -> Just $ DynEncoding KOI8U
    -- ISO-8859-1
    "iso_8859_1"         -> Just $ DynEncoding ISO88591
    "iso8859_1"          -> Just $ DynEncoding ISO88591
    "8859"               -> Just $ DynEncoding ISO88591
    "cp819"              -> Just $ DynEncoding ISO88591
    "csisolatin1"        -> Just $ DynEncoding ISO88591
    "ibm819"             -> Just $ DynEncoding ISO88591
    "iso8859"            -> Just $ DynEncoding ISO88591
    "iso_8859_1_1987"    -> Just $ DynEncoding ISO88591
    "iso_ir_100"         -> Just $ DynEncoding ISO88591
    "l1"                 -> Just $ DynEncoding ISO88591
    "latin"              -> Just $ DynEncoding ISO88591
    "latin1"             -> Just $ DynEncoding ISO88591
    -- ISO-8859-2
    "iso_8859_2"         -> Just $ DynEncoding ISO88592
    "iso8859_2"          -> Just $ DynEncoding ISO88592
    "csisolatin2"        -> Just $ DynEncoding ISO88592
    "iso_8859_2_1987"    -> Just $ DynEncoding ISO88592
    "iso_ir_101"         -> Just $ DynEncoding ISO88592
    "l2"                 -> Just $ DynEncoding ISO88592
    "latin2"             -> Just $ DynEncoding ISO88592
    -- ISO-8859-3
    "iso_8859_3"         -> Just $ DynEncoding ISO88593
    "iso8859_3"          -> Just $ DynEncoding ISO88593
    "csisolatin3"        -> Just $ DynEncoding ISO88593
    "iso_8859_3_1988"    -> Just $ DynEncoding ISO88593
    "iso_ir_109"         -> Just $ DynEncoding ISO88593
    "l3"                 -> Just $ DynEncoding ISO88593
    "latin3"             -> Just $ DynEncoding ISO88593
    --ISO-8859-4
    "iso_8859_4"         -> Just $ DynEncoding ISO88594
    "iso8859_4"          -> Just $ DynEncoding ISO88594
    "csisolatin4"        -> Just $ DynEncoding ISO88594
    "iso_8859_4_1988"    -> Just $ DynEncoding ISO88594
    "iso_ir_110"         -> Just $ DynEncoding ISO88594
    "l4"                 -> Just $ DynEncoding ISO88594
    "latin4"             -> Just $ DynEncoding ISO88594
    --ISO-8859-5
    "iso_8859_5"         -> Just $ DynEncoding ISO88595
    "iso8859_5"          -> Just $ DynEncoding ISO88595
    "csisolatincyrillic" -> Just $ DynEncoding ISO88595
    "cyrillic"           -> Just $ DynEncoding ISO88595
    "iso_8859_5_1988"    -> Just $ DynEncoding ISO88595
    "iso_ir_144"         -> Just $ DynEncoding ISO88595
    -- ISO-8859-6
    "iso_8859_6"         -> Just $ DynEncoding ISO88596
    "iso8859_6"          -> Just $ DynEncoding ISO88596
    "arabic"             -> Just $ DynEncoding ISO88596
    "asmo_708"           -> Just $ DynEncoding ISO88596
    "csisolatinarabic"   -> Just $ DynEncoding ISO88596
    "ecma_114"           -> Just $ DynEncoding ISO88596
    "iso_8859_6_1987"    -> Just $ DynEncoding ISO88596
    "iso_ir_127"         -> Just $ DynEncoding ISO88596
    -- ISO-8859-7
    "iso_8859_7"         -> Just $ DynEncoding ISO88597
    "iso8859_7"          -> Just $ DynEncoding ISO88597
    "csisolatingreek"    -> Just $ DynEncoding ISO88597
    "ecma_118"           -> Just $ DynEncoding ISO88597
    "elot_928"           -> Just $ DynEncoding ISO88597
    "greek"              -> Just $ DynEncoding ISO88597
    "greek8"             -> Just $ DynEncoding ISO88597
    "iso_8859_7_1987"    -> Just $ DynEncoding ISO88597
    "iso_ir_126"         -> Just $ DynEncoding ISO88597
    -- ISO-8859-8
    "iso_8859_8"         -> Just $ DynEncoding ISO88598
    "iso8859_8"          -> Just $ DynEncoding ISO88598
    "csisolatinhebrew"   -> Just $ DynEncoding ISO88598
    "hebrew"             -> Just $ DynEncoding ISO88598
    "iso_8859_8_1988"    -> Just $ DynEncoding ISO88598
    "iso_ir_138"         -> Just $ DynEncoding ISO88598
    -- ISO-8859-9
    "iso_8859_9"         -> Just $ DynEncoding ISO88599
    "iso8859_9"          -> Just $ DynEncoding ISO88599
    "csisolatin5"        -> Just $ DynEncoding ISO88599
    "iso_8859_9_1989"    -> Just $ DynEncoding ISO88599
    "iso_ir_148"         -> Just $ DynEncoding ISO88599
    "l5"                 -> Just $ DynEncoding ISO88599
    "latin5"             -> Just $ DynEncoding ISO88599
    -- ISO-8859-10
    "iso_8859_10"        -> Just $ DynEncoding ISO885910
    "iso8859_10"         -> Just $ DynEncoding ISO885910
    "csisolatin6"        -> Just $ DynEncoding ISO885910
    "iso_8859_10_1992"   -> Just $ DynEncoding ISO885910
    "iso_ir_157"         -> Just $ DynEncoding ISO885910
    "l6"                 -> Just $ DynEncoding ISO885910
    "latin6"             -> Just $ DynEncoding ISO885910
    -- ISO-8859-11
    "iso_8859_11"        -> Just $ DynEncoding ISO885911
    "iso8859_11"         -> Just $ DynEncoding ISO885911
    "thai"               -> Just $ DynEncoding ISO885911
    "iso_8859_11_2001"   -> Just $ DynEncoding ISO885911
    -- ISO-8859-13
    "iso_8859_13"        -> Just $ DynEncoding ISO885913
    "iso8859_13"         -> Just $ DynEncoding ISO885913
    -- ISO-8859-14
    "iso_8859_14"        -> Just $ DynEncoding ISO885914
    "iso8859_14"         -> Just $ DynEncoding ISO885914
    "iso_8859_14_1998"   -> Just $ DynEncoding ISO885914
    "iso_celtic"         -> Just $ DynEncoding ISO885914
    "iso_ir_199"         -> Just $ DynEncoding ISO885914
    "l8"                 -> Just $ DynEncoding ISO885914
    "latin8"             -> Just $ DynEncoding ISO885914
    -- ISO-8859-15
    "iso_8859_15"        -> Just $ DynEncoding ISO885915
    "iso8859_15"         -> Just $ DynEncoding ISO885915
    "latin9"             -> Just $ DynEncoding ISO885915
    "l9"                 -> Just $ DynEncoding ISO885915
    -- ISO-8859-16
    "iso_8859_16"        -> Just $ DynEncoding ISO885916
    "iso8859_16"         -> Just $ DynEncoding ISO885916
    "iso_8859_16_2001"   -> Just $ DynEncoding ISO885916
    "iso_ir_226"         -> Just $ DynEncoding ISO885916
    "l10"                -> Just $ DynEncoding ISO885916
    "latin10"            -> Just $ DynEncoding ISO885916
    -- CP1250
    "cp1250"             -> Just $ DynEncoding CP1250
    "windows_1250"       -> Just $ DynEncoding CP1250
    -- CP1251
    "cp1251"             -> Just $ DynEncoding CP1251
    "windows_1251"       -> Just $ DynEncoding CP1251
    -- CP1252
    "cp1252"             -> Just $ DynEncoding CP1252
    "windows_1252"       -> Just $ DynEncoding CP1252
    -- CP1253
    "cp1253"             -> Just $ DynEncoding CP1253
    "windows_1253"       -> Just $ DynEncoding CP1253
    -- CP1254
    "cp1254"             -> Just $ DynEncoding CP1254
    "windows_1254"       -> Just $ DynEncoding CP1254
    -- CP1255
    "cp1255"             -> Just $ DynEncoding CP1255
    "windows_1255"       -> Just $ DynEncoding CP1255
    -- CP1256
    "cp1256"             -> Just $ DynEncoding CP1256
    "windows_1256"       -> Just $ DynEncoding CP1256
    -- CP1257
    "cp1257"             -> Just $ DynEncoding CP1257
    "windows_1257"       -> Just $ DynEncoding CP1257
    -- CP1258
    "cp1258"             -> Just $ DynEncoding CP1258
    "windows_1258"       -> Just $ DynEncoding CP1258
    -- GB18030
    "gb18030"            -> Just $ DynEncoding GB18030
    "gb18030_2000"       -> Just $ DynEncoding GB18030
    -- MacOSRoman
    "macintosh"          -> Just $ DynEncoding MacOSRoman
    -- JIS X 0201
    "jis_x_0201"         -> Just $ DynEncoding JISX0201
    -- JIS X 0208
    "jis_x_0208"         -> Just $ DynEncoding JISX0208
    -- ISO 2022-JP
    "iso_2022_jp"        -> Just $ DynEncoding ISO2022JP
    -- SJIS
    "sjis"               -> Just $ DynEncoding SJIS
    -- MSDOS codepages
    "cp437"              -> Just $ DynEncoding CP437
    "cp737"              -> Just $ DynEncoding CP737
    "cp775"              -> Just $ DynEncoding CP775
    "cp850"              -> Just $ DynEncoding CP850
    "cp852"              -> Just $ DynEncoding CP852
    "cp855"              -> Just $ DynEncoding CP855
    "cp857"              -> Just $ DynEncoding CP857
    "cp860"              -> Just $ DynEncoding CP860
    "cp861"              -> Just $ DynEncoding CP861
    "cp862"              -> Just $ DynEncoding CP862
    "cp863"              -> Just $ DynEncoding CP863
    "cp864"              -> Just $ DynEncoding CP864
    "cp865"              -> Just $ DynEncoding CP865
    "cp866"              -> Just $ DynEncoding CP866
    "cp869"              -> Just $ DynEncoding CP869
    "cp874"              -> Just $ DynEncoding CP874
    -- defaults to nothing
    _                    -> Nothing
  where
    normalizeEncoding s = map toLower $ subRegex sep s "_"
    sep = mkRegex "[^0-9A-Za-z]+"

-- | Takes the name of an encoding and creates a dynamic encoding from it.
encodingFromString :: String -> DynEncoding
encodingFromString str = maybe
	(error $ "Data.Encoding.encodingFromString: Unknown encoding: "++show str)
	id
	(encodingFromStringExplicit str)
