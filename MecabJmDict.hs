{-# LANGUAGE TemplateHaskell #-}
module MecabJmDict
  where

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
import MecabEntry

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

fRs r
  | not (null $ r ^. readingInfo) = False
  | otherwise = True

fnd = do
  esAll <- getJMDictEntries jmDictFilePath
  let
      es = esAll

  mec <- getMecab

  fh <- openFile "entries" WriteMode
  count <- newIORef $ Map.empty
  void $ forM es $ \e -> do
    let
      ks = filter fKs (e ^.. entryKanjiElements . traverse)
      rs = filter fRs (e ^.. entryReadingElements . traverse)
      isUk = elem UsuallyKana (e ^.. entrySenses . traverse . senseMisc . traverse)

      poss = e ^.. entrySenses . traverse. sensePartOfSpeech . traverse

      kss = if isUk || (null ks)
        then rs ^.. traverse . readingPhrase . to unReadingPhrase
        else ks ^.. traverse . kanjiPhrase . to unKanjiPhrase
    -- when (not $ null kss) $ pPrint kss
    forM (kss) $ \r -> do
      ress <- parseMecab mec r
      let res = filter (isJust . snd) ress
      -- pPrint res
      if (length res > 1)
        then modifyIORef' count (incrementPosMap poss)
          >> T.hPutStrLn fh r
        else return ()
  c <- readIORef count
  let cs =  reverse $ sortBy (comparing snd) $ Map.toList c

  pPrint cs

incrementPosMap ps m = foldl' (\m p -> Map.alter inc p m) m ps
  where inc Nothing = Just 1
        inc (Just v) = Just (v + 1)

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

createMecabEntries :: Entry -> [MecabEntry]
createMecabEntries e = concat $ map mainF vs
  where
    mainF v =
      case pos of
        PosNoun -> [makeNounMecabEntry v]
        PosExpressions -> [makeNounMecabEntry v]
        PosNounType AdverbialNoun -> [makeNounAdvMecabEntry v]
        PosNounType AdjNoun_No -> [makeNounAdjMecabEntry v]
        PosNounType _ -> [makeNounMecabEntry v]

        -- PosAdverb _ ->

        PosAdjective IAdjective
          -> makeAdjMecabEntries v isSuffix
        PosAdjective _
          -> [makeNounAdjMecabEntry v]

        PosVerb (Regular Ichidan) _
          -> makeIchidanMecabEntries v
        PosVerb (Regular (Godan ending))
          _ -> makeGodanMecabEntries v ending
        PosVerb (Irregular SuruI) _
          -> makeSuruIMecabEntries v
        -- PosVerb (Irregular GodanRu) _ ->  -- mostly exp
        -- PosVerb (Special Kureru) _ ->
        -- PosVerb (Special Kuru) _ -> -- mostly exp
        -- PosVerb (Special SuVerb) _ ->
        -- PosVerb (Special SuruS) _ -> -- mostly exp
        -- PosVerb (Irregular RuIrregular) _ ->
        -- PosVerb (Irregular NuIrregular) _ ->
        _ -> []

    defRP = (e ^. entryReadingElements . to (NE.head) . readingPhrase)
    vs = (map findRestReading ks)
      <> (map (\x -> (unReadingPhrase x, x)) rs)

    ks = (e ^.. entryKanjiElements . traverse . kanjiPhrase)

    rs = (e ^.. entryReadingElements . traverse . readingPhrase)

    findRestReading kp = (,) (unKanjiPhrase kp)
      $ maybe defRP _readingPhrase
      $ Map.lookup kp (restrictedKanjiPhrases e)
    v = ("", ReadingPhrase "")
    isSuffix = elem PosSuffix poss
      || elem (PosNounType SuffixNoun) poss
    poss = e ^.. entrySenses . traverse. sensePartOfSpeech . traverse
    pnt = head [x | PosNounType x <- poss]
    pvt = head [x | PosVerb x _ <- poss]
    pos
      | elem PosExpressions poss = PosExpressions
      | elem PosNoun poss = PosNoun
      | otherwise = maybe PosNoun identity
        $ (PosNounType <$> pnt)
        <|> (PosVerb <$> pvt <*> pure Transitive)

restrictedKanjiPhrases :: Entry
  -> Map KanjiPhrase ReadingElement
restrictedKanjiPhrases e = Map.fromList $ concat $
  e ^.. entryReadingElements . traverse
    . to (\re -> re ^.. readingRestrictKanji . traverse
           . to (\kp -> (kp, re)))
-- Stats
--     ( PosNoun
--     , 76793
--     )
-- ,
--     ( PosExpressions
--     , 12334
--     )
-- ,
--     ( PosNounType AdjNoun_No
--     , 2734
--     )
-- ,
--     ( PosNounType NounWithSuru
--     , 2668
--     )
-- ,
--     ( PosVerb ( Regular Ichidan ) NotSpecified
--     , 2215
--     )
-- ,
--     ( PosAdjective IAdjective
--     , 1665
--     )
-- ,
--     ( PosAdjective NaAdjective
--     , 1658
--     )

--     ( PosVerb ( Regular ( Godan RuEnding ) ) NotSpecified
--     , 1448
--     )
-- ,
--     ( PosVerb ( Regular ( Godan SuEnding ) ) NotSpecified
--     , 1073
--     )
-- ,
--     ( PosAdverb Adverb
--     , 1068
--     )
-- ,
--     ( PosVerb ( Regular ( Godan KuEnding ) ) NotSpecified
--     , 723
--     )
-- ,
--     ( PosVerb ( Regular Ichidan ) Transitive
--     , 705
--     )
-- ,
--     ( PosVerb ( Regular ( Godan SuEnding ) ) Transitive
--     , 575
--     )
-- ,
--     ( PosVerb ( Regular ( Godan UEnding ) ) NotSpecified
--     , 565
--     )
-- ,
--     ( PosAdjective PreNominalAdjective
--     , 527
--     )
-- ,
--     ( PosVerb ( Regular ( Godan RuEnding ) ) Intransitive
--     , 477
--     )
-- ,
--     ( PosVerb ( Regular Ichidan ) Intransitive
--     , 335
--     )
-- ,
--     ( PosVerb ( Regular ( Godan MuEnding ) ) NotSpecified
--     , 271
--     )
-- ,
--     ( PosVerb ( Regular ( Godan RuEnding ) ) Transitive
--     , 250
--     )
-- ,
--     ( PosVerb ( Irregular SuruI ) NotSpecified
--     , 233
--     )
-- ,
--     ( PosAdverb Adverb_To
--     , 189
--     )
-- ,
--     ( PosNounType AdverbialNoun
--     , 171
--     )

-- fromList
--     [
--         ( PosNoun
--         , 59716
--         )
--     ,
--         ( PosNounType NounWithSuru
--         , 2399
--         )
--     ,
--         ( PosNounType AdjNoun_No
--         , 2569
--         )
--     ,
--         ( PosNounType AdverbialNoun
--         , 171
--         )
--     ,
--         ( PosNounType SuffixNoun
--         , 33
--         )
--     ,
--         ( PosNounType PrefixNoun
--         , 27
--         )
--     ,
--         ( PosNounType TemporalNoun
--         , 163
--         )
--     ,
--         ( PosNounType ProperNoun
--         , 1
--         )
--     ,
--         ( PosPronoun
--         , 119
--         )
--     ,
--         ( PosVerb ( Regular Ichidan ) Transitive
--         , 705
--         )
--     ,
--         ( PosVerb ( Regular Ichidan ) Intransitive
--         , 335
--         )
--     ,
--         ( PosVerb ( Regular Ichidan ) NotSpecified
--         , 2189
--         )
--     ,
--         ( PosVerb ( Regular ( Godan BuEnding ) ) Transitive
--         , 3
--         )
--     ,
--         ( PosVerb ( Regular ( Godan BuEnding ) ) Intransitive
--         , 19
--         )
--     ,
--         ( PosVerb ( Regular ( Godan BuEnding ) ) NotSpecified
--         , 64
--         )
--     ,
--         ( PosVerb ( Regular ( Godan GuEnding ) ) Transitive
--         , 12
--         )
--     ,
--         ( PosVerb ( Regular ( Godan GuEnding ) ) Intransitive
--         , 9
--         )
--     ,
--         ( PosVerb ( Regular ( Godan GuEnding ) ) NotSpecified
--         , 88
--         )
--     ,
--         ( PosVerb ( Regular ( Godan KuEnding ) ) Transitive
--         , 84
--         )
--     ,
--         ( PosVerb ( Regular ( Godan KuEnding ) ) Intransitive
--         , 137
--         )
--     ,
--         ( PosVerb ( Regular ( Godan KuEnding ) ) NotSpecified
--         , 723
--         )
--     ,
--         ( PosVerb ( Regular ( Godan MuEnding ) ) Transitive
--         , 100
--         )
--     ,
--         ( PosVerb ( Regular ( Godan MuEnding ) ) Intransitive
--         , 67
--         )
--     ,
--         ( PosVerb ( Regular ( Godan MuEnding ) ) BothTransAndIntransitive
--         , 3
--         )
--     ,
--         ( PosVerb ( Regular ( Godan MuEnding ) ) NotSpecified
--         , 271
--         )
--     ,
--         ( PosVerb ( Regular ( Godan NuEnding ) ) Intransitive
--         , 4
--         )
--     ,
--         ( PosVerb ( Regular ( Godan NuEnding ) ) NotSpecified
--         , 5
--         )
--     ,
--         ( PosVerb ( Regular ( Godan RuEnding ) ) Transitive
--         , 250
--         )
--     ,
--         ( PosVerb ( Regular ( Godan RuEnding ) ) Intransitive
--         , 477
--         )
--     ,
--         ( PosVerb ( Regular ( Godan RuEnding ) ) BothTransAndIntransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Regular ( Godan RuEnding ) ) NotSpecified
--         , 1448
--         )
--     ,
--         ( PosVerb ( Regular ( Godan SuEnding ) ) Transitive
--         , 575
--         )
--     ,
--         ( PosVerb ( Regular ( Godan SuEnding ) ) Intransitive
--         , 52
--         )
--     ,
--         ( PosVerb ( Regular ( Godan SuEnding ) ) BothTransAndIntransitive
--         , 1
--         )
--     ,
--         ( PosVerb ( Regular ( Godan SuEnding ) ) NotSpecified
--         , 1073
--         )
--     ,
--         ( PosVerb ( Regular ( Godan TuEnding ) ) Transitive
--         , 13
--         )
--     ,
--         ( PosVerb ( Regular ( Godan TuEnding ) ) Intransitive
--         , 42
--         )
--     ,
--         ( PosVerb ( Regular ( Godan TuEnding ) ) NotSpecified
--         , 170
--         )
--     ,
--         ( PosVerb ( Regular ( Godan UEnding ) ) Transitive
--         , 72
--         )
--     ,
--         ( PosVerb ( Regular ( Godan UEnding ) ) Intransitive
--         , 108
--         )
--     ,
--         ( PosVerb ( Regular ( Godan UEnding ) ) NotSpecified
--         , 565
--         )
--     ,
--         ( PosVerb ( Irregular SuruI ) Transitive
--         , 4
--         )
--     ,
--         ( PosVerb ( Irregular SuruI ) Intransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Irregular SuruI ) NotSpecified
--         , 233
--         )
--     ,
--         ( PosVerb ( Irregular RuIrregular ) Intransitive
--         , 8
--         )
--     ,
--         ( PosVerb ( Irregular NuIrregular ) Intransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Irregular NuIrregular ) NotSpecified
--         , 2
--         )
--     ,
--         ( PosVerb ( Irregular GodanRu ) Intransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Irregular GodanRu ) NotSpecified
--         , 57
--         )
--     ,
--         ( PosVerb ( Special Kureru ) NotSpecified
--         , 2
--         )
--     ,
--         ( PosVerb ( Special GodanAru ) Intransitive
--         , 1
--         )
--     ,
--         ( PosVerb ( Special GodanAru ) NotSpecified
--         , 2
--         )
--     ,
--         ( PosVerb ( Special IkuYuku ) Intransitive
--         , 11
--         )
--     ,
--         ( PosVerb ( Special IkuYuku ) NotSpecified
--         , 53
--         )
--     ,
--         ( PosVerb ( Special GodanUEnding ) NotSpecified
--         , 5
--         )
--     ,
--         ( PosVerb ( Special Kuru ) Intransitive
--         , 5
--         )
--     ,
--         ( PosVerb ( Special Kuru ) NotSpecified
--         , 66
--         )
--     ,
--         ( PosVerb ( Special SuVerb ) Intransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Special SuVerb ) NotSpecified
--         , 4
--         )
--     ,
--         ( PosVerb ( Special SuruS ) Transitive
--         , 28
--         )
--     ,
--         ( PosVerb ( Special SuruS ) Intransitive
--         , 27
--         )
--     ,
--         ( PosVerb ( Special SuruS ) BothTransAndIntransitive
--         , 2
--         )
--     ,
--         ( PosVerb ( Special SuruS ) NotSpecified
--         , 59
--         )
--     ,
--         ( PosVerb ( Special Zuru ) Transitive
--         , 8
--         )
--     ,
--         ( PosVerb ( Special Zuru ) Intransitive
--         , 3
--         )
--     ,
--         ( PosVerb ( Special Zuru ) NotSpecified
--         , 13
--         )
--     ,
--         ( PosAdverb Adverb
--         , 1068
--         )
--     ,
--         ( PosAdverb Adverb_To
--         , 189
--         )
--     ,
--         ( PosAdjective IAdjective
--         , 1608
--         )
--     ,
--         ( PosAdjective NaAdjective
--         , 1564
--         )
--     ,
--         ( PosAdjective PreNounAdjective
--         , 107
--         )
--     ,
--         ( PosAdjective TaruAdjective
--         , 166
--         )
--     ,
--         ( PosAdjective NariAdjective
--         , 1
--         )
--     ,
--         ( PosAdjective PreNominalAdjective
--         , 527
--         )
--     ,
--         ( PosAdjective YoiIiAdjective
--         , 93
--         )
--     ,
--         ( PosAdjective KuAdjective
--         , 6
--         )
--     ,
--         ( PosAdjective ShikuAdjective
--         , 1
--         )
--     ,
--         ( PosNumeric
--         , 6
--         )
--     ,
--         ( PosCounter
--         , 13
--         )
--     ,
--         ( PosAuxiliary Auxiliary
--         , 9
--         )
--     ,
--         ( PosAuxiliary AuxiliaryVerb
--         , 17
--         )
--     ,
--         ( PosAuxiliary AuxiliaryAdjective
--         , 3
--         )
--     ,
--         ( PosExpressions
--         , 11456
--         )
--     ,
--         ( PosIntejection
--         , 101
--         )
--     ,
--         ( PosSuffix
--         , 64
--         )
--     ,
--         ( PosPrefix
--         , 32
--         )
--     ,
--         ( PosConjugation
--         , 82
--         )
--     ,
--         ( PosParticle
--         , 6
--         )
--     ]
