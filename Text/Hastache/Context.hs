-- Module:      Text.Hastache.Context
-- Copyright:   Sergey S Lymar (c) 2011 
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable

{- | 
Hastache context helpers
-}
module Text.Hastache.Context (
      mkStrContext
    , mkGenericContext
    ) where 

import Data.Data
import Data.Generics
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

import Text.Hastache

x ~> f = f $ x
infixl 9 ~>

-- | Make Hastache context from String -> MuType function
mkStrContext :: Monad m => (String -> MuType m) -> MuContext m
mkStrContext f a = decodeStr a ~> f

{- | 
Make Hastache context from Data.Data deriving type

Supported field types:

 * String
 
 * Char
 
 * Double

 * Float

 * Int

 * Int8

 * Int16

 * Int32

 * Int64

 * Integer

 * Word

 * Word8

 * Word16

 * Word32

 * Word64

 * Data.ByteString.ByteString

 * Data.ByteString.Lazy.ByteString
 
 * Data.Text.Text

 * Data.Text.Lazy.Text
 
 * Bool
 
 * Data.ByteString.ByteString -> Data.ByteString.ByteString
 
 * String -> String
 
 * Data.ByteString.ByteString -> Data.ByteString.Lazy.ByteString
 
 * MonadIO m => Data.ByteString.ByteString -> m Data.ByteString.ByteString
 
 * MonadIO m => String -> m String
 
 * MonadIO m => Data.ByteString.ByteString -> m Data.ByteString.Lazy.ByteString

Example:

@
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LZ 
import Data.Data 
import Data.Generics 
import Data.Char

data InternalData = InternalData {
    someField       :: String,
    anotherField    :: Int
    } deriving (Data, Typeable, Show)

data Example = Example {
    stringField             :: String,
    intField                :: Int,
    dataField               :: InternalData,
    simpleListField         :: [String],
    dataListField           :: [InternalData],
    stringFunc              :: String -> String,
    byteStringFunc          :: B.ByteString -> B.ByteString,
    monadicStringFunc       :: String -> IO String,
    monadicByteStringFunc   :: B.ByteString -> IO B.ByteString
    } deriving (Data, Typeable)

example = hastacheStr defaultConfig (encodeStr template) 
    (mkGenericContext context)
    where
    template = concat $ map (++ \"\\n\") [
        \"string: {{stringField}}\",
        \"int: {{intField}}\",
        \"data: {{dataField.someField}}, {{dataField.anotherField}}\",
        \"data: {{#dataField}}{{someField}}, {{anotherField}}{{/dataField}}\",
        \"simple list: {{#simpleListField}}{{.}} {{/simpleListField}}\",
        \"data list:\",
        \"{{#dataListField}}\",
        \" * {{someField}}, {{anotherField}}. top level var: {{intField}}\",
        \"{{/dataListField}}\",
        \"{{#stringFunc}}upper{{/stringFunc}}\",
        \"{{#byteStringFunc}}reverse{{/byteStringFunc}}\",
        \"{{#monadicStringFunc}}upper (monadic){{/monadicStringFunc}}\",
        \"{{#monadicByteStringFunc}}reverse (monadic){{/monadicByteStringFunc}}\"]
    context = Example { stringField = \"string value\", intField = 1, 
        dataField = InternalData \"val\" 123, simpleListField = [\"a\",\"b\",\"c\"],
        dataListField = [InternalData \"aaa\" 1, InternalData \"bbb\" 2],
        stringFunc = map toUpper,
        byteStringFunc = B.reverse,
        monadicStringFunc = return . map toUpper,
        monadicByteStringFunc = return . B.reverse }

main = example >>= LZ.putStrLn
@

Result:

@
string: string value 
int: 1 
data: val, 123 
data: val, 123 
simple list: a b c  
data list: 
 * aaa, 1. top level var: 1 
 * bbb, 2. top level var: 1 
UPPER 
esrever 
UPPER (MONADIC)
)cidanom( esrever
@

-}
mkGenericContext :: (Monad m, Data a, Typeable1 m) => a -> MuContext m
mkGenericContext val = toGenTemp val ~> convertGenTempToContext
    
data TD m = 
      TSimple (MuType m) 
    | TObj [(String, TD m)] 
    | TList [TD m] 
    | TUnknown
    deriving (Show)

toGenTemp :: (Data a, Monad m, Typeable1 m) => a -> TD m
toGenTemp a = zip fields (gmapQ procField a) ~> TObj
    where
    fields = toConstr a ~> constrFields

procFieldMuVariable :: (MuVar a, Monad m) => a -> TD m
procFieldMuVariable i = muVariable i ~> TSimple

procFieldString :: (Monad m) => String -> TD m
procFieldString i = muVariable (encodeStr i) ~> TSimple

procFieldChar :: (Monad m) => Char -> TD m
procFieldChar = procFieldMuVariable

procFieldDouble :: (Monad m) => Double -> TD m
procFieldDouble = procFieldMuVariable

procFieldFloat :: (Monad m) => Float -> TD m
procFieldFloat = procFieldMuVariable

procFieldInt :: (Monad m) => Int -> TD m
procFieldInt = procFieldMuVariable

procFieldInt8 :: (Monad m) => Int8 -> TD m
procFieldInt8 = procFieldMuVariable

procFieldInt16 :: (Monad m) => Int16 -> TD m
procFieldInt16 = procFieldMuVariable

procFieldInt32 :: (Monad m) => Int32 -> TD m
procFieldInt32 = procFieldMuVariable

procFieldInt64 :: (Monad m) => Int64 -> TD m
procFieldInt64 = procFieldMuVariable

procFieldInteger :: (Monad m) => Integer -> TD m
procFieldInteger = procFieldMuVariable

procFieldWord :: (Monad m) => Word -> TD m
procFieldWord = procFieldMuVariable

procFieldWord8 :: (Monad m) => Word8 -> TD m
procFieldWord8 = procFieldMuVariable

procFieldWord16 :: (Monad m) => Word16 -> TD m
procFieldWord16 = procFieldMuVariable

procFieldWord32 :: (Monad m) => Word32 -> TD m
procFieldWord32 = procFieldMuVariable

procFieldWord64 :: (Monad m) => Word64 -> TD m
procFieldWord64 = procFieldMuVariable

procFieldBSByteString :: (Monad m) => BS.ByteString -> TD m
procFieldBSByteString = procFieldMuVariable

procFieldLBSByteString :: (Monad m) => LBS.ByteString -> TD m
procFieldLBSByteString = procFieldMuVariable

procFieldTextText :: (Monad m) => Text.Text -> TD m
procFieldTextText = procFieldMuVariable

procFieldLTextText :: (Monad m) => LText.Text -> TD m
procFieldLTextText = procFieldMuVariable

procFieldBool :: (Monad m) => Bool -> TD m
procFieldBool i = MuBool i ~> TSimple

procField :: (Data a, Monad m, Typeable1 m) => a -> TD m
procField = 
    obj
    `ext1Q` list
    `extQ` procFieldString
    `extQ` procFieldChar
    `extQ` procFieldDouble
    `extQ` procFieldFloat
    `extQ` procFieldInt
    `extQ` procFieldInt8
    `extQ` procFieldInt16
    `extQ` procFieldInt32
    `extQ` procFieldInt64
    `extQ` procFieldInteger
    `extQ` procFieldWord
    `extQ` procFieldWord8
    `extQ` procFieldWord16
    `extQ` procFieldWord32
    `extQ` procFieldWord64
    `extQ` procFieldBSByteString
    `extQ` procFieldLBSByteString
    `extQ` procFieldTextText
    `extQ` procFieldLTextText
    `extQ` procFieldBool
    
    `extQ` muLambdaBSBS
    `extQ` muLambdaSS
    `extQ` muLambdaBSLBS
    
    `extQ` muLambdaMBSBS
    `extQ` muLambdaMSS
    `extQ` muLambdaMBSLBS
    where
    obj a = case dataTypeRep (dataTypeOf a) of
        AlgRep [c] -> toGenTemp a
        _ -> TUnknown
    list a = map procField a ~> TList

    muLambdaBSBS :: (BS.ByteString -> BS.ByteString) -> TD m
    muLambdaBSBS f = muLambda f ~> TSimple

    muLambdaSS :: (String -> String) -> TD m
    muLambdaSS f = muLambda fd ~> TSimple
        where
        fd s = decodeStr s ~> f

    muLambdaBSLBS :: (BS.ByteString -> LBS.ByteString) -> TD m
    muLambdaBSLBS f = muLambda f ~> TSimple

    -- monadic

    muLambdaMBSBS :: (Monad m) => (BS.ByteString -> m BS.ByteString) -> TD m
    muLambdaMBSBS f = muLambdaM f ~> TSimple

    muLambdaMSS :: (Monad m) => (String -> m String) -> TD m
    muLambdaMSS f = muLambdaM fd ~> TSimple
        where
        fd s = decodeStr s ~> f

    muLambdaMBSLBS :: (Monad m) => (BS.ByteString -> m LBS.ByteString) -> TD m
    muLambdaMBSLBS f = muLambdaM f ~> TSimple

convertGenTempToContext :: TD t -> MuContext t
convertGenTempToContext v = mkMap "" Map.empty v ~> mkMapContext
    where
    mkMap name m (TSimple t) = Map.insert (encodeStr name) t m
    mkMap name m (TObj lst) = foldl (foldTObj name) m lst ~>
        Map.insert (encodeStr name) 
        ([foldl (foldTObj "") Map.empty lst ~> mkMapContext] ~> MuList)
    mkMap name m (TList lst) = Map.insert (encodeStr name) 
        (map convertGenTempToContext lst ~> MuList) m
    mkMap _ m _ = m
    
    mkName name newName = if length name > 0 
        then concat [name, ".", newName]
        else newName
    foldTObj name m (fn, fv) = mkMap (mkName name fn) m fv
    
    mkMapContext m a = case Map.lookup a m of
        Nothing -> 
            case a == dotBS of
                True -> 
                    case Map.lookup BS.empty m of
                        Nothing -> MuNothing
                        Just a -> a
                _ -> MuNothing
        Just a -> a

dotBS = encodeStr "."

