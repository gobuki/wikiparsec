{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import WikiPrelude
import Test.HUnit
import Text.MediaWiki.Wiktionary.Base
import Text.MediaWiki.Wiktionary.English

solderEntry :: Text
solderEntry = unlines [
   "==English==",
   "{{slim-wikipedia}}",
   "[[Image:60-40 Solder.jpg|thumb|right|A spool of solder.]]",
   "",
   "===Etymology===",
   "From {{etyl|enm|en}} {{m|enm|solderen}}, from {{etyl|fro|en}} {{m|fro|solder}} (Modern French {{m|fr|souder}}) from {{etyl|la|en}} {{m|la|solidāre}}, present active infinitive of {{m|la|solidō||make solid}}.",
   "",
   "===Pronunciation===",
   "{{der-top|Pronunciation}}",
   "* {{enPR|sŏl'də(r)}}, {{IPA|/ˈsɒldə(ɹ)/|lang=en}}",
   "* {{rhymes|ɒldə(ɹ)|lang=en}}",
   "* {{audio|en-uk-solder.ogg|Audio (UK)|lang=en}}",
   "* {{enPR|sōl'də(r)}}, {{IPA|/ˈsəʊldə(ɹ)/|lang=en}}",
   "* {{rhymes|əʊldə(ɹ)|lang=en}}",
   "* {{audio|en-au-solder.ogg|Audio (AUS)|lang=en}}",
   "* {{audio|en-ca-solder.ogg|Audio (CA)|lang=en}}",
   "* {{a|CanE|GenAm}} {{enPR|sŏd′ər}}, {{IPA|/ˈsɑdɚ/|lang=en}}",
   "* {{rhymes|ɒdə(ɹ)|lang=en}}",
   "* {{audio|en-us-solder.ogg|Audio (US)|lang=en}}",
   "* {{enPR|sōd'ə(r)}}, {{IPA|/ˈsəʊdə(ɹ)/|lang=en}}",
   "* {{rhymes|əʊdə(ɹ)|lang=en}}",
   "{{der-bottom}}",
   "* [http://www.languagehat.com/archives/001382.php LanguageHat discussion on the many pronunciations of “solder”]",
   "",
   "===Noun===",
   "{{en-noun|-}}",
   "",
   "# Any of various [[alloy]]s, often of [[tin]] and [[lead]], that are used to join small pieces of [[metal]] together",
   "",
   "====Translations====",
   "{{trans-top|any of various alloys that are used to join small pieces of metal together}}",
   "* Armenian: {{t+|hy|զոդ}}",
   "* Chinese:",
   "*: Mandarin: {{t+|cmn|焊料|tr=hànliào|sc=Hani}}",
   "* Czech: {{t+|cs|pájka|f}}",
   "* Danish: {{t|da|loddemetal|n}}",
   "* Finnish: {{t|fi|juote}}, {{t|fi|juotosmetalli}}, {{t|fi|juotostina}}",
   "* French: {{t+|fr|soudure|f}}",
   "* German: {{t|de|Lötmetall|n}}, {{t+|de|Lot|n}}, {{t|de|Lötdraht|m}}, {{t+|de|Lötzinn|n}}",
   "* Greek: {{t+|el|συγκολλώ}}",
   "* Icelandic: {{t|is|lóðmálmur|m}}, {{t|is|lóðtin|n}}, {{t|is|brasmálmur|m}}, {{t|is|loðningarefni|n}}",
   "* Japanese: {{t|ja|半田|tr=はんだ, handa|sc=Jpan}}",
   "{{trans-mid}}",
   "* Korean: {{t+|ko|땜납|tr=ddaemnap|sc=Kore}}",
   "* Latvian: {{t|lv|lode|f}}",
   "* Persian: {{t+|fa|لحیم|tr=lahim|sc=fa-Arab}}",
   "* Polish: {{t+|pl|cyna|f}}, {{t|pl|spoiwo lutownicze|n}} {{qualifier|formal}}, {{t+|pl|lut|m}}",
   "* Portuguese: {{t+|pt|solda|f}}",
   "* Russian: {{t+|ru|припо́й|m}}",
   "* Serbo-Croatian: {{t+|sh|lem|m}}",
   "* Spanish: {{t|es|estaño para soldar|m}}, {{t+|es|estaño|m}}",
   "* Turkish: {{t+|tr|lehim}}",
   "* Welsh: {{t+|cy|sodr|m}}",
   "{{trans-bottom}}",
   "",
   "{{checktrans-top}}",
   "* Welsh: {{t-check|cy|sodr}}",
   "{{trans-bottom}}",
   "",
   "===Verb===",
   "{{en-verb}}",
   "",
   "# {{senseid|en|to join with solder}} To join with (or as if with) solder.",
   "",
   "====Antonyms====",
   "* [[desolder]]",
   "",
   "====Derived terms====",
   "* [[solderer]]",
   "* [[soldering iron]]",
   "",
   "====Translations====",
   "{{trans-top|to join with solder}}",
   "* Arabic: {{t-needed|ar}}",
   "* Armenian: {{t+|hy|զոդել}}",
   "* Bulgarian: {{t|bg|запоя́вам|sc=Cyrl}}, {{t+|bg|споя́вам|sc=Cyrl}}",
   "* Chinese:",
   "*: Mandarin: {{t+|cmn|焊接|tr=hànjiē|sc=Hani}}",
   "* Czech: {{t|cs|pájet}}, {{t|cs|letovat}}",
   "* Danish: {{t|da|lodde}}",
   "* Dutch: {{t+|nl|solderen}}",
   "* Esperanto: {{t|eo|luti}}",
   "* Finnish: {{t+|fi|juottaa}}",
   "* French: {{t+|fr|souder}}",
   "* Georgian: {{t-needed|ka}}",
   "* German: {{t+|de|löten}}",
   "* Hebrew: {{t-needed|he}}",
   "* Hindi: {{t-needed|hi}}",
   "* Hungarian: {{t+|hu|forraszt}}",
   "* Icelandic: {{t|is|lóða}}, {{t|is|brasa}}, {{t|is|sjóða saman}}, {{t+|is|kveikja}}, {{t|is|kveikja saman}}",
   "{{trans-mid}}",
   "* Japanese: {{t|ja|はんだづけ|tr=handadzuke suru|alt=はんだづけする|sc=Jpan}}",
   "* Korean: {{t-needed|ko}}",
   "* Latin: {{t+|la|plumbō}}",
   "* Latvian: {{t|lv|lodēt}}",
   "* Macedonian: {{t|mk|леми|tr=lémi}}",
   "* Persian: {{t|fa|لحیم کردن|tr=lahim kardan|sc=fa-Arab}}",
   "* Polish: {{t+|pl|lutować}}",
   "* Portuguese: {{t+|pt|soldar}}",
   "* Quechua: {{t|qu|chapiy}}",
   "* Romanian: {{t-needed|ro}}",
   "* Russian: {{t+|ru|пая́ть|impf}}, {{t+|ru|припая́ть|pf}}, {{t+|ru|спая́ть|pf}}",
   "* Serbo-Croatian: {{t+|sh|zalemiti}}",
   "* Spanish: {{t+|es|soldar}}",
   "* Swedish: {{t+|sv|löda}}",
   "* Thai: {{t-needed|th}}",
   "* Turkish: {{t+|tr|lehimlemek}}",
   "* Vietnamese: {{t+|vi|hàn}}",
   "{{trans-bottom}}",
   "",
   "===See also===",
   "* [[braze]]",
   "* [[flux]]",
   "* [[weld]]",
   "",
   "===Anagrams===",
   "* [[resold#English|resold]]",
   "",
   "[[Category:en:Alloys]]",
   "",
   "----",
   "",
   "==French==",
   "",
   "===Etymology===",
   "Cf. {{m|fr|solde}}.",
   "",
   "===Verb===",
   "{{fr-verb}}",
   "",
   "# to [[close]] (a deal)",
   "# {{label|fr|finance}} to [[settle]], to [[pay off]] (debt)",
   "# to [[sell]] at sales, to have a [[sale]]",
   "# {{label|fr|reflexive|~ '''par'''}} to [[end up]] (in), to [[result]] (in)",
   "",
   "====Conjugation====",
   "{{fr-conj-auto}}",
   "",
   "====Related terms====",
   "* [[solde#French|solde]]",
   "* [[solderie]]",
   "* [[soldeur]]",
   "",
   "===Anagrams===",
   "* [[drôles#French|drôles]]",
   "",
   "===External links===",
   "* {{R:TLFi}}"
   ]

solderFacts :: [WiktionaryFact]
solderFacts = [
    WiktionaryFact "derived/etym" (term ["-","enm"]) (term ["solder","en","","1"]),
    WiktionaryFact "related/etym" (term ["solder","en","","1"]) (term ["solderen","enm"]),
    WiktionaryFact "derived/etym" (term ["-","fro"]) (term ["solder","en","","1"]),
    WiktionaryFact "related/etym" (term ["solder","en","","1"]) (term ["solder","fro"]),
    WiktionaryFact "related/etym" (term ["solder","en","","1"]) (term ["souder","fr"]),
    WiktionaryFact "derived/etym" (term ["-","la"]) (term ["solder","en","","1"]),
    WiktionaryFact "related/etym" (term ["solder","en","","1"]) (term ["solidare","la"]),
    WiktionaryFact "related/etym" (term ["solder","en","","1"]) (term ["solido","la"]),
    WiktionaryFact "definition" (term ["solder","en","n","1","def.1"]) (term ["Any of various alloys, often of tin and lead, that are used to join small pieces of metal together","en"]),
    WiktionaryFact "link" (term ["solder","en","n","1","def.1"]) (term ["alloy"]),
    WiktionaryFact "link" (term ["solder","en","n","1","def.1"]) (term ["tin"]),
    WiktionaryFact "link" (term ["solder","en","n","1","def.1"]) (term ["lead"]),
    WiktionaryFact "link" (term ["solder","en","n","1","def.1"]) (term ["metal"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["զոդ","hy"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["焊料","cmn"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["pájka","cs"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["loddemetal","da"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["juote","fi"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["juotosmetalli","fi"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["juotostina","fi"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["soudure","fr"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["Lötmetall","de"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["Lot","de"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["Lötdraht","de"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["Lötzinn","de"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["συγκολλω","el"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lóðmálmur","is"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lóðtin","is"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["brasmálmur","is"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["loðningarefni","is"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["半田","ja"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["땜납","ko"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lode","lv"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["لحیم","fa"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["cyna","pl"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["spoiwo lutownicze","pl"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lut","pl"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["solda","pt"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["припой","ru"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lem","sh"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["estaño para soldar","es"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["estaño","es"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["lehim","tr"]),
    WiktionaryFact "translation" (term ["solder","en","n","1","any of various alloys that are used to join small pieces of metal together"]) (term ["sodr","cy"]),
    WiktionaryFact "definition" (term ["solder","en","v","1","to join with solder"]) (term ["To join with (or as if with) solder","en"]),
    WiktionaryFact "antonym" (term ["solder","en","v","1"]) (term ["desolder"]),
    WiktionaryFact "derived" (term ["solder","en","v","1"]) (term ["solderer"]),
    WiktionaryFact "derived" (term ["solder","en","v","1"]) (term ["soldering iron"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["զոդել","hy"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["запоявам","bg"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["споявам","bg"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["焊接","cmn"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["pájet","cs"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["letovat","cs"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["lodde","da"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["solderen","nl"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["luti","eo"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["juottaa","fi"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["souder","fr"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["löten","de"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["forraszt","hu"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["lóða","is"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["brasa","is"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["sjóða saman","is"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["kveikja","is"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["kveikja saman","is"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["はんだづけ","ja"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["plumbo","la"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["lodēt","lv"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["леми","mk"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["لحیم کردن","fa"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["lutować","pl"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["soldar","pt"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["chapiy","qu"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["паять","ru"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["припаять","ru"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["спаять","ru"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["zalemiti","sh"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["soldar","es"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["löda","sv"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["lehimlemek","tr"]),
    WiktionaryFact "translation" (term ["solder","en","v","1","to join with solder"]) (term ["hàn","vi"]),
    WiktionaryFact "related" (term ["solder","en","","1"]) (term ["braze"]),
    WiktionaryFact "related" (term ["solder","en","","1"]) (term ["flux"]),
    WiktionaryFact "related" (term ["solder","en","","1"]) (term ["weld"]),
    WiktionaryFact "related/etym" (term ["solder","fr","","1"]) (term ["solde","fr"]),
    WiktionaryFact "definition" (term ["solder","fr","v","1","def.1"]) (term ["to close (a deal)","en"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.1"]) (term ["close"]),
    WiktionaryFact "definition" (term ["solder","fr","v","1","def.2"]) (term ["to settle, to pay off (debt)","en"]),
    WiktionaryFact "context" (term ["solder","fr","v","1","def.2"]) (term ["finance","en"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.2"]) (term ["settle"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.2"]) (term ["pay off"]),
    WiktionaryFact "definition" (term ["solder","fr","v","1","def.3"]) (term ["to sell at sales, to have a sale","en"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.3"]) (term ["sell"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.3"]) (term ["sale"]),
    WiktionaryFact "definition" (term ["solder","fr","v","1","def.4"]) (term ["to end up (in), to result (in)","en"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.4"]) (term ["end up"]),
    WiktionaryFact "link" (term ["solder","fr","v","1","def.4"]) (term ["result"]),
    WiktionaryFact "related" (term ["solder","fr","v","1"]) (term ["solde","fr"]),
    WiktionaryFact "related" (term ["solder","fr","v","1"]) (term ["solderie"]),
    WiktionaryFact "related" (term ["solder","fr","v","1"]) (term ["soldeur"])]

testExtract :: (Eq a, Show a) => (Text -> a) -> Text -> a -> Test
testExtract func input output = (cs input) ~: (func input) ~?= output

compareLists :: (Eq a, Show a) => String -> [a] -> [a] -> [Test]
compareLists name input output =
  -- This deliberately runs off the end of the list. If the lists are the
  -- same length, then indexing both of them by (length input) will
  -- confirm this by successfully comparing Nothing to Nothing.
  [(name <> ": item " <> (show i)) ~: (index input i) ~?= (index output i) | i <- [0..(length input)]]

defnTests = [
    -- Parse a definition list with word senses

    testExtract (parseDefinitions "en" enTemplates (termPos "en" "test" "Noun"))
                "# {{senseid|en|first}} definition 1\n# {{senseid|en|second}} definition 2\n# definition 3"
                [WiktionaryFact "definition" (term ["test", "en", "Noun", "", "first"]) (simpleTerm "en" "definition 1"),
                 WiktionaryFact "definition" (term ["test", "en", "Noun", "", "second"]) (simpleTerm "en" "definition 2"),
                 WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.3"]) (simpleTerm "en" "definition 3")],

    -- testExtract (parseDefinitions "en" enTemplates (termPos "en" "test" "Noun"))
    --             "# Definition of bonjour, a French word.\n\
    --             \#: A first example?\n\
    --             \#:: An English translation of the first example.\n\
    --             \#: A second example\n\
    --             \#:: Romanization for example (Can be ignored).\n\
    --             \#:: An English translation of the second example."
    --             [WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.1"]) (simpleTerm "en" "Definition of bonjour, a French word"),
    --              WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.1.ex.1"]) (simpleTerm "en" "A first example?"),
    --              WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.1.ex.1.t"]) (simpleTerm "en" "An English translation of the first example"),
    --              WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.1.ex.2"]) (simpleTerm "en" "A second example"),
    --              WiktionaryFact "definition" (term ["test", "en", "Noun", "", "def.1.ex.2.t"]) (simpleTerm "en" "An English translation of the second example")],

    testExtract (parseDefinitions "en" enTemplates (termPos "fr" "test" "Interjection"))
                    "# Definition of bonjour, a French word.\n\
                    \#: Bonjour, les enfants.\n\
                    \#:: Hello, children.\n\
                    \#: Bonjour, comment allez-vous?\n\
                    \#:: Romanization for example (Can be ignored).\n\
                    \#:: Hello, how do you do?\n\
                    \#: {{ux|fr|Un autre exemple.|t=Another example.}}"
                    [WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1"]) (simpleTerm "en" "Definition of bonjour, a French word"),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.1"]) (simpleTerm "fr" "Bonjour, les enfants."),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.1.t"]) (simpleTerm "en" "Hello, children."),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.2"]) (simpleTerm "fr" "Bonjour, comment allez-vous?"),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.2.t"]) (simpleTerm "en" "Hello, how do you do?"),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.3"]) (simpleTerm "fr" "Un autre exemple."),
                    WiktionaryFact "definition" (term ["test", "fr", "Interjection", "", "def.1.ex.3.t"]) (simpleTerm "en" "Another example.")
                    ],

    testExtract (parseDefinitions "en" enTemplates (termPos "ko" "test" "Determiner"))
                    "# [[new]]\n\
                    \#: '''새''' 집으로 벌써 이사했어요?\n\
                    \#:: '''''Sae''' jip-euro beolsseo isahaess-eoyo?''\n\
                    \#::: Did you already move into your '''new''' house?"
                  [WiktionaryFact "definition" (term ["test","ko", "Determiner", "", "def.1"]) (simpleTerm "en" "new"),
                  WiktionaryFact "link" (term ["test","ko", "Determiner", "", "def.1"]) (term ["new"]),
                  WiktionaryFact "definition" (term ["test","ko", "Determiner", "", "def.1.ex.1"]) (simpleTerm "ko" "새 집으로 벌써 이사했어요?"),
                  WiktionaryFact "definition" (term ["test","ko", "Determiner", "", "def.1.ex.1.t"]) (simpleTerm "en" "Did you already move into your new house?")
                  ],


    -- Parse a "related terms" list, where the first entry has an additional link we ignore
    testExtract (enParseWiktionary "example")
                "==English==\n===Related terms===\n* [[entry]] with an [[addendum]]\n* [[other]]"
                [WiktionaryFact "related" (term ["example", "en", "", "1"]) (term ["entry"]),
                 WiktionaryFact "related" (term ["example", "en", "", "1"]) (term ["other"])],

    -- Parse a "compound" template two different ways
    testExtract (enParseWiktionary "placeholder")
                "==English==\n===Etymology===\n{{compound|en|place|holder}}"
                [WiktionaryFact "derived" (term ["place", "en"]) (term ["placeholder", "en", "", "1"]),
                 WiktionaryFact "derived" (term ["holder", "en"]) (term ["placeholder", "en", "", "1"])],

    testExtract (enParseWiktionary "placeholder")
                "==English==\n===Etymology===\n{{compound|place|holder|lang=en}}"
                [WiktionaryFact "derived" (term ["place", "en"]) (term ["placeholder", "en", "", "1"]),
                 WiktionaryFact "derived" (term ["holder", "en"]) (term ["placeholder", "en", "", "1"])]
    ]

entryTests = compareLists "Example entry for 'solder'" (enParseWiktionary "solder" solderEntry) solderFacts

tests = test (defnTests ++ entryTests)

main :: IO ()
main = void (runTestTT tests)
