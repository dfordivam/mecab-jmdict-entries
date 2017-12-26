module MecabEntry where

import Protolude hiding (to, (&))
import           Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Control.Monad.Writer
import Control.Exception (assert)
import Text.MeCab
import qualified Data.ByteString.Lazy as BSL

-- data MecabEntry = MecabEntry
--   { _mecabEntrySurface :: Text
--   , _mecabEntryNumber  :: Int -- 2 and 3 field, same number
--   , _mecabEntryRanking :: Int -- lower value means higher priority
--   , _mecabEntryMainPOS :: MecabMainPOS
--   , _mecabEntryPOS1    :: Maybe MecabPOS1
--   , _mecabEntryPOS2    :: Maybe MecabPOS2
--   , _mecabEntryPOS3    :: Maybe MecabPOS3
--   , _mecabEntryRule    :: Maybe MecabRule
--   , _mecabEntryConj    :: Maybe MecabConj
--   , _mecabEntryDictForm :: Text
--   , _mecabEntryReading  :: Text
--   , _mecabEntryPronunciation  :: Text
--   }
--   deriving (Show)

-- 1 見づら～いっ
-- 2 ,19
-- 3 ,19
-- 4 ,6795
-- 5 ,形容詞
-- 6 ,自立
-- 7 ,*
-- 8 ,*
-- 9 ,形容詞・アウオ段
-- 10,基本形
-- 11,見づらい
-- 12,ミヅラーイッ
-- 13,ミズラーイッ

type ParseWrM = Writer
  ((Set Int -- t23
  , Set Int) -- t4i
  , Set Text -- t5
  , (Set (Maybe Text) -- t6
  , Set (Maybe Text) -- t7
  , Set (Maybe Text) -- t8
  , Set (Maybe Text) -- t9
  , Set (Maybe Text))) -- t10


makeMecabSets fp = do
  fc <- T.readFile fp
  return (parseAll $ T.lines fc)
  where
    parseAll ls = execWriter (mapM parseOneLine ls)
    parseOneLine :: Text -> ParseWrM ()
    parseOneLine l = case T.splitOn "," l of
      (t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:t11:t12:t13:[]) ->
        tell ((t23, Set.singleton t4i), t5v
             , (h t6, h t7, h t8, h t9, h t10))
        where
          t23 = assert (t2i == t3i) $ Set.singleton $ t2i
          (Just t2i) = readMaybe $ T.unpack t2
          (Just t3i) = readMaybe $ T.unpack t3
          (Just t4i) = readMaybe $ T.unpack t4
          t5v = assert (not $ T.null t5) $ Set.singleton $ t5
          h t = Set.singleton $ case t of
            "*" -> Nothing
            _ -> Just t

-- Verbs.csv
-- t2, t3 (561, 1280)
-- t4 (2731, 15396)
-- t5 動詞
-- t6 "接尾", "自立", "非自立"
-- t7 Nothing
-- t8 Nothing
-- t9
    -- [ "カ変・クル"
    -- , "カ変・来ル"
    -- , "サ変・−スル"
    -- , "サ変・−ズル"
    -- , "サ変・スル"
    -- , "ラ変"
    -- , "一段"
    -- , "一段・クレル"
    -- , "一段・得ル"
    -- , "上二・ダ行"
    -- , "上二・ハ行"
    -- , "下二・カ行"
    -- , "下二・ガ行"
    -- , "下二・ダ行"
    -- , "下二・ハ行"
    -- , "下二・マ行"
    -- , "下二・得"
    -- , "五段・カ行イ音便"
    -- , "五段・カ行促音便"
    -- , "五段・カ行促音便ユク"
    -- , "五段・ガ行"
    -- , "五段・サ行"
    -- , "五段・タ行"
    -- , "五段・ナ行"
    -- , "五段・バ行"
    -- , "五段・マ行"
    -- , "五段・ラ行"
    -- , "五段・ラ行特殊"
    -- , "五段・ワ行ウ音便"
    -- , "五段・ワ行促音便"
    -- , "四段・サ行"
    -- , "四段・タ行"
    -- , "四段・ハ行"
    -- , "四段・バ行"
-- t10
    -- [ "仮定形"
    -- , "仮定縮約１"
    -- , "体言接続"
    -- , "体言接続特殊"
    -- , "体言接続特殊２"
    -- , "命令ｅ"
    -- , "命令ｉ"
    -- , "命令ｒｏ"
    -- , "命令ｙｏ"
    -- , "基本形"
    -- , "文語基本形"
    -- , "未然ウ接続"
    -- , "未然ヌ接続"
    -- , "未然レル接続"
    -- , "未然形"
    -- , "未然特殊"
    -- , "現代基本形"
    -- , "連用タ接続"
    -- , "連用形"


-- Adj.csv
-- t2, t3 (11, 146)
-- t4 (842, 9609)
-- t5 形容詞
-- t6 "接尾"
-- t7 Nothing
-- t8 Nothing
-- t9 "不変化型"
-- t10 "ガル接続"

-- Noun.csv
-- t2, t3 (1285, 1285)
-- t4 (-19662, 16360)
-- t5 名詞
-- t6 "一般"
-- t7 Nothing
-- t8 Nothing
-- t9 Nothing
-- t10 Nothing

-- Adverb.csv
-- t5 副詞
-- t6 "一般", "助詞類接続"

