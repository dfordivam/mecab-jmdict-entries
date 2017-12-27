{-# LANGUAGE ParallelListComp #-}

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

data MecabEntry = MecabEntry
  { _mecabEntrySurface :: Text
  , _mecabEntryNumber  :: Int -- 2 and 3 field, same number
  , _mecabEntryRanking :: Int -- lower value means higher priority
  , _mecabEntryMainPOS :: Text -- t5
  , _mecabEntryPOS     :: Maybe Text -- t6
  , _mecabEntryRule    :: Maybe Text -- t9
  , _mecabEntryConj    :: Maybe Text -- t10
  , _mecabEntryDictForm :: Text -- t11 -- same as JMDict kanjiPhrase/readingPhrase
  , _mecabEntryReading  :: Text -- t12,t13 Katakana
  }
  deriving (Show)

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
-- ( fromList
--     [ Just "接尾"
--     , Just "自立"
--     , Just "非自立"
--     ]
-- , fromList [ Nothing ]
-- , fromList [ Nothing ]
-- , fromList
--     [ Just "不変化型"
--     , Just "形容詞・アウオ段"
--     , Just "形容詞・イイ"
--     , Just "形容詞・イ段"
--     ]
-- , fromList
--     [ Just "ガル接続"
--     , Just "仮定形"
--     , Just "仮定縮約１"
--     , Just "仮定縮約２"
--     , Just "体言接続"
--     , Just "命令ｅ"
--     , Just "基本形"
--     , Just "基本形-促音便"
--     , Just "文語基本形"
--     , Just "未然ウ接続"
--     , Just "未然ヌ接続"
--     , Just "連用ゴザイ接続"
--     , Just "連用タ接続"
--     , Just "連用テ接続"
--     ]
-- )

-- 赤い,19,19,4961,形容詞,自立,*,*,形容詞・アウオ段,基本形,赤い,アカイ,アカイ
-- 赤し,23,23,5085,形容詞,自立,*,*,形容詞・アウオ段,文語基本形,赤い,アカシ,アカシ
-- 赤から,27,27,4962,形容詞,自立,*,*,形容詞・アウオ段,未然ヌ接続,赤い,アカカラ,アカカラ
-- 赤かろ,25,25,4961,形容詞,自立,*,*,形容詞・アウオ段,未然ウ接続,赤い,アカカロ,アカカロ
-- 赤かっ,33,33,4961,形容詞,自立,*,*,形容詞・アウオ段,連用タ接続,赤い,アカカッ,アカカッ
-- 赤く,35,35,4961,形容詞,自立,*,*,形容詞・アウオ段,連用テ接続,赤い,アカク,アカク
-- 赤くっ,35,35,4961,形容詞,自立,*,*,形容詞・アウオ段,連用テ接続,赤い,アカクッ,アカクッ
-- 赤う,31,31,4965,形容詞,自立,*,*,形容詞・アウオ段,連用ゴザイ接続,赤い,アカウ,アカー
-- 赤ぅ,31,31,4961,形容詞,自立,*,*,形容詞・アウオ段,連用ゴザイ接続,赤い,アカゥ,アカー
-- 赤き,21,21,4961,形容詞,自立,*,*,形容詞・アウオ段,体言接続,赤い,アカキ,アカキ
-- 赤けれ,13,13,4961,形容詞,自立,*,*,形容詞・アウオ段,仮定形,赤い,アカケレ,アカケレ
-- 赤かれ,29,29,4961,形容詞,自立,*,*,形容詞・アウオ段,命令ｅ,赤い,アカカレ,アカカレ
-- 赤けりゃ,15,15,4961,形容詞,自立,*,*,形容詞・アウオ段,仮定縮約１,赤い,アカケリャ,アカケリャ
-- 赤きゃ,17,17,4961,形容詞,自立,*,*,形容詞・アウオ段,仮定縮約２,赤い,アカキャ,アカキャ
-- 赤,11,11,5836,形容詞,自立,*,*,形容詞・アウオ段,ガル接続,赤い,アカ,アカ


-- 小ざかしい,43,43,4682,形容詞,自立,*,*,形容詞・イ段,基本形,小ざかしい,コザカシイ,コザカシイ
-- 小ざかし,45,45,4682,形容詞,自立,*,*,形容詞・イ段,文語基本形,小ざかしい,コザカシ,コザカシ
-- 小ざかしから,47,47,4682,形容詞,自立,*,*,形容詞・イ段,未然ヌ接続,小ざかしい,コザカシカラ,コザカシカラ
-- 小ざかしかろ,46,46,4682,形容詞,自立,*,*,形容詞・イ段,未然ウ接続,小ざかしい,コザカシカロ,コザカシカロ
-- 小ざかしかっ,50,50,4682,形容詞,自立,*,*,形容詞・イ段,連用タ接続,小ざかしい,コザカシカッ,コザカシカッ
-- 小ざかしく,51,51,4682,形容詞,自立,*,*,形容詞・イ段,連用テ接続,小ざかしい,コザカシク,コザカシク
-- 小ざかしくっ,51,51,4682,形容詞,自立,*,*,形容詞・イ段,連用テ接続,小ざかしい,コザカシクッ,コザカシクッ
-- 小ざかしゅう,49,49,4682,形容詞,自立,*,*,形容詞・イ段,連用ゴザイ接続,小ざかしい,コザカシュウ,コザカシュー
-- 小ざかしゅぅ,49,49,4682,形容詞,自立,*,*,形容詞・イ段,連用ゴザイ接続,小ざかしい,コザカシュゥ,コザカシュー
-- 小ざかしき,44,44,4682,形容詞,自立,*,*,形容詞・イ段,体言接続,小ざかしい,コザカシキ,コザカシキ
-- 小ざかしけれ,40,40,4682,形容詞,自立,*,*,形容詞・イ段,仮定形,小ざかしい,コザカシケレ,コザカシケレ
-- 小ざかしかれ,48,48,4682,形容詞,自立,*,*,形容詞・イ段,命令ｅ,小ざかしい,コザカシカレ,コザカシカレ
-- 小ざかしけりゃ,41,41,4682,形容詞,自立,*,*,形容詞・イ段,仮定縮約１,小ざかしい,コザカシケリャ,コザカシケリャ
-- 小ざかしきゃ,42,42,4682,形容詞,自立,*,*,形容詞・イ段,仮定縮約２,小ざかしい,コザカシキャ,コザカシキャ
-- 小ざかし,39,39,4682,形容詞,自立,*,*,形容詞・イ段,ガル接続,小ざかしい,コザカシ,コザカシ


-- 赤う,31,31,4965,形容詞,自立,*,*,形容詞・アウオ段,連用ゴザイ接続,赤い,アカウ,アカー
-- 赤ぅ,31,31,4961,形容詞,自立,*,*,形容詞・アウオ段,連用ゴザイ接続,赤い,アカゥ,アカー
-- 小ざかしゅう,49,49,4682,形容詞,自立,*,*,形容詞・イ段,連用ゴザイ接続,小ざかしい,コザカシュウ,コザカシュー
-- 小ざかしゅぅ,49,49,4682,形容詞,自立,*,*,形容詞・イ段,連用ゴザイ接続,小ざかしい,コザカシュゥ,コザカシュー
makeAdjMecabEntries
  :: (Surface, ReadingPhrase)
  -> Bool
  -> [MecabEntry]
makeAdjMecabEntries (s,rp) isSuffix =
  [MecabEntry s i 6956 "形容詞" t6 t9 t10 s t12
   | s <- surfaces
   | t10 <- adjTypes
   | t12 <- readings]

  where
    i = 40
    stem = assert (T.last s == 'い') (T.init s)
    t6 = Just $ if isSuffix
      then "接尾"
      else "自立"
    t9 = Just $ if aou
      then "形容詞・アウオ段"
      else "形容詞・イ段"

    endings = (if aou then aouEndings else iEndings)
    surfaces = map (\e -> stem <> e) endings
    readings = map (\e -> (getKatakana rp) <> (toKatakana e)) endings

    aou = not $ elem (T.last stem) ['し', 'じ', 'ち', 'き']
    adjTypes = map Just
      [ "基本形"
      , "文語基本形"
      , "未然ヌ接続"
      , "未然ウ接続"
      , "連用タ接続"
      , "連用テ接続"
      , "連用テ接続"
      , "連用ゴザイ接続"
      , "連用ゴザイ接続"
      , "体言接続"
      , "仮定形"
      , "命令ｅ"
      , "仮定縮約１"
      , "仮定縮約２"
      , "ガル接続"]

    aouEndings =
      [ "い"
      , "し"
      , "から"
      , "かろ"
      , "かっ"
      , "く"
      , "くっ"
      , "う"
      , "ぅ"
      , "き"
      , "けれ"
      , "かれ"
      , "けり"
      , "きゃ"
      , "" ]

    iEndings =
      [ "い"
      , ""
      , "から"
      , "かろ"
      , "かっ"
      , "く"
      , "くっ"
      , "ゅう"
      , "ゅぅ"
      , "き"
      , "けれ"
      , "かれ"
      , "けり"
      , "きゃ"
      , "" ]







      
-- Noun.csv
-- t2, t3 (1285, 1285)
-- t4 (-19662, 16360)
-- t5 名詞
-- t6 "一般"
-- t7 Nothing
-- t8 Nothing
-- t9 Nothing
-- t10 Nothing

type Surface = Text
-- For expressions, always make noun entry
makeNounMecabEntry
  :: (Surface, ReadingPhrase)
  -> MecabEntry
makeNounMecabEntry (s, rp) = MecabEntry
  s
  1285
  7650
  "名詞"
  (Just "一般")
  Nothing
  Nothing
  s
  (getKatakana rp)

-- Noun.adj.csv
-- t5 名詞
-- t6 形容動詞語幹
makeNounAdjMecabEntry
  :: (Surface, ReadingPhrase)
  -> MecabEntry
makeNounAdjMecabEntry (s, rp) = MecabEntry
  s
  1285
  7650
  "名詞"
  (Just "形容動詞語幹")
  Nothing
  Nothing
  s
  (getKatakana rp)

-- Noun.adverbal.csv
-- t5 名詞
-- t6 副詞可能

makeNounAdvMecabEntry
  :: (Surface, ReadingPhrase)
  -> MecabEntry
makeNounAdvMecabEntry (s, rp) = MecabEntry
  s
  1285
  7650
  "名詞"
  (Just "副詞可能")
  Nothing
  Nothing
  s
  (getKatakana rp)

-- Adverb.csv
-- t5 副詞
-- t6 "一般", "助詞類接続"

makeAdvMecabEntry
  :: (Surface, ReadingPhrase)
  -> MecabEntry
makeAdvMecabEntry (s, rp) = MecabEntry
  s
  1285
  7650
  "副詞"
  (Just "一般")
  Nothing
  Nothing
  s
  (getKatakana rp)
