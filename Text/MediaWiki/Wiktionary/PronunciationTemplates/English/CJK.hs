{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Text.MediaWiki.Wiktionary.PronunciationTemplates.English.CJK where 

import WikiPrelude
import Text.MediaWiki.Templates
import Text.MediaWiki.AnnotatedText
import Text.SplitUtils
import Text.MediaWiki.ParseTools
import Text.MediaWiki.Sections
import Text.MediaWiki.WikiText
import Text.MediaWiki.Wiktionary.Base
import Data.Attoparsec.Text
import Data.LanguageNames
import Data.Char hiding (toUpper)
import qualified Data.Char as Ch

import Data.Pronunciation.EnWiktionary.CH_TW
import Data.Maybe
import qualified Data.Map as M

zhpronParseMandarinSubarguments :: Parser [(ProcessedPinyin, [Text])] -- should be ZhPron
zhpronParseMandarinSubarguments = do
  string "|m="
  rawPronunciations <- mandarinUnnamedSubarguments
  let labeledMT = concatMap zhpronLabelMT rawPronunciations
      -- unprocessed = map (\x -> (ProcessedPinyin (fst x) Nothing Nothing, ["Standard Chinese", snd x])) labeledMT
  modifications <- mandarinNamedSubarguments
  
  
  -- let rawPronunciationsWithRegions = 
  return []


mandarinUnnamedSubarguments :: Parser [RawPinyin]
mandarinUnnamedSubarguments = 
     many ( do
            fieldVal <- takeWhile1 (\x -> x /= ',' && x /= '=')
            ((char ',' >> return ()) <|> endOfInput)-- fail if '='
            return fieldVal
          )



mandarinNamedSubarguments :: Parser [MandarinNamedArg]
mandarinNamedSubarguments = 
  manyTill (madarinLabelChange <|> mandarinTonelessVariant) endOfInput



data MandarinNamedArg = MandarinLabelChange {pronunNumber :: Int, labelNumber :: Int, new :: Text}
                                  | Detoner {pronunNumber :: Int, endIndex :: Int}

data ZhPron = ZhPronMandarin ProcessedPinyin [Text]
            | SomethingElse Text

type RawPinyin = Text

data ProcessedPinyin = ProcessedPinyin 
                                        { pinyin :: Text
                                        , pronounced :: Maybe Text
                                        , tonelessVariant :: Maybe Text
                                        , labels :: [Text] }

processRawPinyins :: [Text] -> [MandarinNamedArg] -> [ProcessedPinyin]
processRawPinyins [] _ = []
processRawPinyins raws namedArgs =
  let -- (Lots of steps)
      -- Add labels for Mandarin-Taiwanese variants
      labeledMT = concatMap zhpronLabelMT raws
      f arg (tnChanges,lbChanges) =
        case arg of 
          Detoner num endIx -> (M.insertWith (++) num [endIx] tnChanges, lbChanges)
          MandarinLabelChange num labelNum labelText -> (tnChanges,  M.insert num (labelNum, labelText) lbChanges)
      (tnChanges, lbChanges) = foldr f (M.empty, M.empty) namedArgs
      compare1st a b = compare (fst a) (fst b)
      padded len list = 
        if length list >= len 
          then list 
          else list ++ ["" | _ <- [0..(len - length list - 1)]]
      relabel (ix, raw, originalLabels) (labelNum, labelText) = 
        (ix, raw , foldr (\a b -> (if fst a == labelNum then labelText else snd a) : b ) 
                [] 
                $ zip [0..] (padded (labelNum - 1) originalLabels))
      -- Apply label changes
      relabeled = [ case lookup ix lbChanges of
                      Just (labelNum, labelText) ->  relabel (ix, raw, originalLabels) (labelNum, labelText)
                      Nothing -> (ix, raw, originalLabels)
                  | (ix, (raw, originalLabels)) <- zip [0..] (labeledMT) ] 
      -- Add toneless variants
      detoned = [ case lookup ix tnChanges of
                    Just endIxs -> (raw, Just (detoneMandarinNthsFromLast endIxs raw), labels )
                    Nothing -> (raw, Nothing, labels)
                | (ix, raw, labels) <- relabeled ]
      -- Process tone sandhis

  in []




data MandarinToneSandhi = SandhiBu
                        | SandhiYi
                        | Cluster {initials::Text, vowels::String, finals :: Text, tone :: Int, toneIndex :: Int, unchanging::Bool}
                        deriving (Show, Eq)
                  
parseMandarinToneSandhi ::  Parser [MandarinToneSandhi]
parseMandarinToneSandhi = 
  many  (
    (char '不' >> return SandhiBu)
    <|> (char '一' >> return SandhiYi)
    <|> parseMandarinToneSandhiCluster
    ) 

-- doesn't exactly distinguish border between final and the following initial,
-- (..) but enough to reconstruct the string with adjusted pronunciation
parseMandarinToneSandhiCluster :: Parser MandarinToneSandhi
parseMandarinToneSandhiCluster = do
  unchanging <- (char '#' >> return True) <|> (return False)
  initials <- takeWhile isMandarinConsonant
  (ix, tn, vowelChars) <- parseMandarinVowels
  finals <- takeWhile isMandarinConsonant
  skipWhile (\c -> isSpace c || c `elem` ['\'','-'])
  return Cluster {initials=initials, vowels=vowelChars, finals = finals, tone = tn, toneIndex = ix, unchanging=unchanging}

isMandarinConsonant :: Char -> Bool
isMandarinConsonant = (flip elem)  mandarinConsonants

isMandarinVowel :: Char -> Bool
isMandarinVowel = (flip elem) mandarinVowels

mandarinConsonants :: [Char]
mandarinConsonants = 
  let letters = ['a'..'z'] ++ ['A'..'Z']
  in filter  (not . ((flip elem)) mandarinVowels) letters

mandarinVowels :: [Char]
mandarinVowels = fst <$> mandarinTones

parseMandarinVowels :: Parser (Int,Int,String)
parseMandarinVowels = do
  vowels <- many1 (satisfy isMandarinVowel)
  return $ fst $ foldr f ((0,0,""), length vowels - 1) vowels 
  where f ch ((ix,tn,right), current_ix) =
          case lookup ch mandarinToneInfo of
            Just (ch', 0) -> ((ix,0,ch':right), current_ix - 1)
            Just (ch', q) -> ((current_ix,q,ch':right), current_ix - 1)
            Nothing -> error "Expected vowel with corresponding tone information"
          
          


mandarinToneInfo :: M.Map Char (Char,Int)
mandarinToneInfo = M.fromList mandarinTones

mandarinTones :: [(Char,(Char,Int))]
mandarinTones = mandarinTonesLower ++  ((\x -> (Ch.toUpper (fst x), (Ch.toUpper ((fst . snd) x), ((snd . snd) x)))) <$> mandarinTonesLower)
mandarinTonesLower = [ ('ā', ('a', 1)), ('á', ('a', 2)), ('ǎ', ('a', 3)), ('à', ('a', 4)), ('a',('a',0)), 
                      ('ō', ('o', 1)), ('ó', ('o', 2)), ('ǒ', ('o', 3)), ('ò', ('o', 4)), ('o',('o',0)), 
                      ('ē', ('e', 1)), ('é', ('e', 2)), ('ě', ('e', 3)), ('è', ('e', 4)), ('e',('e',0)), 
                      ('ī', ('i', 1)), ('í', ('i', 2)), ('ǐ', ('i', 3)), ('ì', ('i', 4)), ('i',('i',0)), 
                      ('ū', ('u', 1)), ('ú', ('u', 2)), ('ǔ', ('u', 3)), ('ù', ('u', 4)), ('u',('u',0)), 
                      ('ǖ', ('ü', 1)), ('ǘ', ('ü', 2)), ('ǚ', ('ü', 3)), ('ǜ', ('ü', 4)), ('ü',('ü',0))
                     ] 



-- toneMandarin = M.fromList [ ('ā', 'a'), ('á', 'a'), ('ǎ', 'a'), ('à', 'a'),
--                                ('ō', 'o'), ('ó', 'o'), ('ǒ', 'o'), ('ò', 'o'), 
--                                ('ē', 'e'), ('é', 'e'), ('ě', 'e'), ('è', 'e'),
--                                ('ī', 'i'), ('í', 'i'), ('ǐ', 'i'), ('ì', 'i'), 
--                                ('ū', 'u'), ('ú', 'u'), ('ǔ', 'u'), ('ù', 'u'), 
--                                ('ǖ', 'ü'), ('ǘ', 'ü'), ('ǚ', 'ü'), ('ǜ', 'ü')
--                              ]

-- processMandarinToneSandhis :: Text -> (Text,Text)
-- processMandarinToneSandhis raw = 
--   foldr f ("","") raw 
--   where f ch (base, pronounced) =

  

-- processRawPinyin 


-- processRawPinyin 


zhpronLabelMT :: Text -> [(Text,  [Text])]
zhpronLabelMT raw =
        map g $ foldr f [("", "")] (unpack raw)
        -- map (both pack ) $ filter ((/="") . snd) $ foldr f [("", "")] (unpack raw)
        where f ch acc = 
                case (lookup ch zhpronLabelMTMap) of 
                  Just pairs -> [ (chPron ++ str, lbl1) 
                                | (chPron,lbl1) <- pairs, (str, lbl2) <- acc
                                , (lbl2 == "") || ( lbl1) == lbl2 ]
                  Nothing -> (\x -> (ch : (fst x) , (snd x))) <$> acc
              g (i,j) = (pack i, ("Standard Chinese"::Text):(if j /= "" then [pack j] else []))


zhpronLabelMTMap :: Map Char [(String,String)]
zhpronLabelMTMap = 
  M.fromList (zipWithPronuns <$> mt_tag)
  where zipWithPronuns (symbol, tags) = 
          case lookup symbol mt  of 
            Just pronuns -> (symbol, zip pronuns tags)
            Nothing -> error ("Could not find " ++ [symbol] ++ " in MT")
  

-- processMandarinRegion :: RawPinyin -> (RawPinyin, Map Int Text)
-- processMandarinRegion raw = 
--   let 



----
--- Experimental: 

---

-- handleZhPronunTemplate :: Template -> AnnotatedText
-- handleZhPronunTemplate t =
--   let mandarin = get "m" t
--       mandarinPinyinVariants 




-- Parses the value for field |m=...|
-- Returns something like : 
--  [("shuōfú","Pinyin:Mainland.Standard"),("shuìfú","Pinyin:Taiwan.Standard"), ("shuìfú","Pinyin:Mainland.Variant")]
-- getMandarinVariants :: Text -> [(Text, Text)]
-- getMandarinVariants = 

-- zhpronParseMandarinSubarguments :: Parser ([SimpleArg],[NamedArg])
-- zhpronParseMandarinSubarguments = do 




-- -- Then we get all the named subarguments, which are really just rules for 
-- -- either adding labels (performed first) or adding toneless variants (performed second)
-- zhpronParseMandarin_NamedSubarguments :: Parser ( [PronunciationQualified] -> [PronunciationQualified]
--                                                 , [PronunciationQualified] -> [PronunciationQualified]
--                                                 )
-- zhpronParseMandarin_NamedSubarguments = 


(<..>) = skipWhile isSpace

madarinLabelChange :: Parser MandarinNamedArg
madarinLabelChange = do 
  (<..>)
  basePronuncationPlacement <- decimal 
  char 'n'
  letter <- satisfy (\x -> x ∈ ['a'..'d'])
  let labelNumber =
          case letter of
            'a' -> 1
            'b' -> 2
            'c' -> 3
            'd' -> 4
            _ ->  error "Unexpected label letter."
  (<..>)
  char '='
  (<..>)
  text <- takeWhile1 (\x -> x /= ',') 
  return $ MandarinLabelChange 
            {pronunNumber = (basePronuncationPlacement - 1),
            labelNumber = labelNumber,
            new = text
            }

mandarinTonelessVariant :: Parser MandarinNamedArg
mandarinTonelessVariant = do
  (<..>)
  basePronuncationPlacement <- decimal <|> return 1
  (string "tl" <|> string "ertl")
  tonelessEndIndex <- decimal <|> return 1
  (<..>)
  string "=y"
  (<..>)
  skipWhile (==',')
  (<..>)
  return $ Detoner (basePronuncationPlacement - 1) tonelessEndIndex


  -- if basePronuncationPlacement <= originalCount
  --   then
  --     return (\l -> l)
  --   else 
  --     return (\l -> l)
  -- where f placement charEndIndex collec = 
  --         let referencedBase = lookup placement (basePronunciations collec) in
  --         case referencedBase of 
  --           Just (x,y,z) -> addNewPronun (detoneMandarinNthFromLast charEndIndex x , y, (\t -> "(toneless variant) " <> t) <$> z) collec
  --           Nothing -> collec
  
-- data ZhpronMandarinModifier = PronunLabel {pronunIndex :: Int , labelIndex :: Int , labelText :: Text }
--                             | TonelessVariant {pronunIndex:: Int , toneEndIndex:: Int}

detoneMandarinNthsFromLast :: [Int] -> Text -> Text
detoneMandarinNthsFromLast ns s = 
  (pack . snd) (foldr g (0,"") (unpack s)) 
  where g char (counter, rightward) =
          if M.member char detonedMandarin 
            then  ( counter + 1  
                  , if ((counter + 1 ) `elem` ns) 
                      then (fromMaybe char (M.lookup char detonedMandarin)):rightward 
                      else char:rightward 
                  )
            else (counter, char:rightward)

detonedMandarin :: Map Char Char
detonedMandarin = M.fromList $ (\x -> (fst x, (fst . snd) x)) <$> mandarinTones

-- data PronunciationCollection = PronunciationCollection { basePronunciations:: Map Int PronunciationQualified, additionalPronunciations:: [PronunciationQualified]}
-- addNewPronun :: PronunciationQualified -> PronunciationCollection -> PronunciationCollection
-- addNewPronun x PronunciationCollection {basePronunciations=b,additionalPronunciations=a} = PronunciationCollection b (a++[x])

-- type PronunciationQualified = (PronunciationText, PronunciationType, [PronunciationLabel]) 
-- type PronunciationText = Text --i.e. "shuōfú"
-- type PronunciationType = Text --i.e. "Pinyin"
-- type PronunciationLabel = Text -- i.e. "Standard in Taiwan" ; "Varient in Mainland"


-- IGNORED : Sichuanese , Taishanese, Teochew
zhPronLanguages :: [(Text, Language)]
zhPronLanguages = [ ("m","cmn") -- Mandarin
                  , ("dg","dng") -- Dungan
                  , ("c","yue") -- Cantonese
                  , ("g","gan") -- Gan
                  , ("h","hak") -- Hakka
                  , ("j","cjy") -- Jin
                  , ("mb","mnp") -- Min Bei
                  , ("md","cdo") -- Min Dong
                  , ("mn","nan") -- Min Nan
                  , ("w","wuu") -- Wu
                  , ("x","hsn") -- Xiang
                  ]

-- type PronunType = Text
-- type BasePronun = Text
-- type PhoneticPronun = Text
-- type VariationPronuns = [Text]
-- type MandarinPronunciation = (PronunType, BasePronun, Maybe PhoneticPronun, VariationPronuns)
-- type LabeledMandarinPronunciation = (MandarinPronunciation, Text)



