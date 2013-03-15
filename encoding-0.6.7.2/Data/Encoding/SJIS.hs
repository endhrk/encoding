{-# LANGUAGE DeriveDataTypeable #-}
{- | Implements the japanese character encoding Shift_JIS.
     See http://www.iana.org/assignments/charset-reg/shift_jis for reference.
 -}
module Data.Encoding.SJIS where

import Data.Typeable

import Data.Encoding.Base
import Data.Encoding.Exception
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Data.Encoding.ASCII
import Data.Encoding.JISX0201
import Data.Encoding.ShiftJIS

import Control.Throws

data SJIS = SJIS deriving (Eq,Show,Typeable)

instance Encoding SJIS where
    encodeChar = encodeCharSJIS
    decodeChar = decodeCharSJIS
    encode = encodeSJIS
    decode = decodeSJIS
    encodeable _ c = encodeable JISX0201 c || encodeable ShiftJIS c

encodingForChar _ c
    | encodeable JISX0201 c = Just $ DynEncoding JISX0201
    | encodeable ShiftJIS c = Just $ DynEncoding ShiftJIS
    | otherwise = Nothing

readEncode w
    | 33  <= w && w <= 126 = Just $ DynEncoding JISX0201
    | 161 <= w && w <= 223 = Just $ DynEncoding JISX0201
    | 129 <= w && w <= 159 = Just $ DynEncoding ShiftJIS
    | 224 <= w && w <= 239 = Just $ DynEncoding ShiftJIS
    | otherwise            = Nothing


encodeCharSJIS :: ByteSink m => SJIS -> Char -> m ()
encodeCharSJIS e c = case encodingForChar e c of
                          Nothing -> throwException (HasNoRepresentation c)
                          Just enc -> do
                                    encodeChar enc c

decodeCharSJIS :: ByteSource m => SJIS -> m Char
decodeCharSJIS e = do
  w <- fetchAhead fetchWord8
  case (readEncode w) of
    Nothing -> decodeChar ASCII
    Just renc -> decodeChar renc

encodeSJIS :: ByteSink m => SJIS -> String -> m ()
encodeSJIS e = encode' (DynEncoding ASCII)
    where
      encode' _ [] = return ()
      encode' enc (c:cs) = case encodingForChar e c of
                             Nothing -> throwException (HasNoRepresentation c)
                             Just nenc
                                  | enc==nenc -> do
                                                 encodeChar enc c
                                                 encode' enc cs
                                  | otherwise -> do
                                                 encodeChar nenc c
                                                 encode' nenc cs

decodeSJIS :: ByteSource m => SJIS -> m String
decodeSJIS e = decode' (DynEncoding ASCII)
    where
      decode' enc = do
        empty <- sourceEmpty
        if empty
          then return []
          else (do
                 w <- fetchAhead fetchWord8
                 case (readEncode w) of
                   Just renc -> decode' renc
                   Nothing -> do
                             c <- decodeChar enc
                             cs <- decode' enc
                             return (c:cs)
               )

