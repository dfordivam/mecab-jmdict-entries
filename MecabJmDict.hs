{-# LANGUAGE TemplateHaskell #-}
module MecabJmDict
  where

import Protolude hiding (to, (&))
import           Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import Text.Read
import Data.JMDict.AST
import Data.Char
import Text.Pretty.Simple
import qualified Data.Conduit.List
import qualified Data.JMDict.XML.Parser as X
import Data.JMDict.AST.Parser hiding (isKana)
import Text.XML.Stream.Parse hiding (anyOf)
import Control.Monad.Trans.Resource
import NLP.Japanese.Utils
import Data.Binary
import Data.Conduit

import Text.MeCab
import qualified Data.ByteString.Lazy as BSL

jmDictFilePath = "/home/divam/nobup/jmdict/JMdict_e"

getMecab =  new ["mecab", "-d"
      , "/home/divam/nobup/mecab-tools/mecab-ipadic-neologd-output-files"]

fKs k
  | not (null $ k ^. kanjiInfo) = False
  | any (\p -> elem p [Gai1, Gai2]) (k ^. kanjiPriority)
     = False
  | T.last (unKanjiPhrase $ k ^. kanjiPhrase) == 'て' = False
  | otherwise = True

fnd = do
  esAll <- getJMDictEntries jmDictFilePath
  let
      es = esAll

  mec <- getMecab

  count <- newIORef 0
  void $ forM es $ \e -> do
    let
      ks = filter fKs (e ^.. entryKanjiElements . traverse)
      rs = (e ^.. entryReadingElements . traverse . readingPhrase . to unReadingPhrase)
      isUk = elem UsuallyKana (e ^.. entrySenses . traverse . senseMisc . traverse)

      kss = if isUk
        then rs
        else ks ^.. traverse . kanjiPhrase . to unKanjiPhrase
    -- when (not $ null kss) $ pPrint kss
    forM (kss) $ \r -> do
      ress <- parseMecab mec r
      let res = filter (isJust . snd) ress
      -- pPrint res
      if (length res > 1)
        then modifyIORef' count (+ 1)
        else return ()
  print =<< readIORef count


getJMDictEntries :: FilePath -> IO [Entry]
getJMDictEntries fp =
  runResourceT $ parseFile parseSetting fp
    $$ X.parseJMDict
    .| Data.Conduit.List.map makeAST
    .| Data.Conduit.List.mapMaybe (rightToMaybe)
    .| Data.Conduit.List.consume

parseMecab :: MeCab -> Text -> IO ([(Text, Maybe MecabNodeFeatures)])
parseMecab m t = do
  nodes <- parseToNodes m t
  let feats = map nodeFeature nodes
  return $ zip (map nodeSurface nodes)
    (fmap makeMecabFeat feats)

makeMecabFeat :: Text -> Maybe MecabNodeFeatures
makeMecabFeat n = case T.splitOn "," n of
  ("BOS/EOS":_) -> Nothing
  (t1:t2:t3:t4:t5:t6:t7:t8:t9:[]) -> Just $
    MecabNodeFeatures
      (getFeat t1)
      (getMaybeFeat t2)
      (getMaybeFeat t3)
      (getMaybeFeat t4)
      (getMaybeFeat t5)
      (getMaybeFeat t6)
      (getFeat t7)
      (getFeat t8)
      (getFeat t9)
  _ -> Nothing
  where
    getFeat tf
      | tf == "*" = error $ "getFeat-> " <> n
      | otherwise = tf
    getMaybeFeat tf
      | tf == "*" = Nothing
      | otherwise = Just tf

data MecabNodeFeatures = MecabNodeFeatures
  { _mecabNodeFeat1 :: Text
  , _mecabNodeFeat2 :: Maybe Text
  , _mecabNodeFeat3 :: Maybe Text
  , _mecabNodeFeat4 :: Maybe Text
  , _mecabNodeFeat5 :: Maybe Text
  , _mecabNodeFeat6 :: Maybe Text
  , _mecabNodeFeat7 :: Text
  , _mecabNodeFeat8 :: Text
  , _mecabNodeFeat9 :: Text
  }
  deriving (Show)

makeLenses ''MecabNodeFeatures
