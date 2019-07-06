import Text.Parsec
import System.Environment

-- gramatika
data Document = Document [Record]
data Record = Record Leader [Field]
data Leader = Leader Code Value
data Field = Field Code [Indicator] [Subfield]
data Indicator = Indicator Char
data Subfield = Subfield Code Value
type Value = String
type Code = String

-- shorthand za UNIMARC nevidljive znakove
unitSeparator :: Char
unitSeparator = '\US'

rowSeparator :: Char
rowSeparator = '\RS'

groupSeparator :: Char
groupSeparator = '\GS'

-- shorthand za carriage return / new line karaktere
carriageReturn :: Char
carriageReturn = '\r'

newLine :: Char
newLine = '\n'


-- odavde pocinje program
main :: IO()
main = do
  (inputFilePath:outputFilePath:[]) <- getArgs -- argumenti komandne linije
  inputFileContent <- readFile inputFilePath -- sadrzaj fajla koji se parsira (koristice se za internu tokenizaciju stringa)
  case (runParser parseDocument 0 inputFilePath inputFileContent) of -- nesto nalik switch-u, radi se nad rezultatom parsera koji parsira uz funkciju parseDocument i ima vrednost unutrasnjeg state-a 0
      Left error -> putStrLn (show error) -- matchuje rezultat sa Left -> ukoliko jeste -> to je error -> ispisujemo
      Right parseResult -> writeFile outputFilePath (show parseResult) -- matchuje Rezultat sa right -> to je rezultat parsiranja (lista rekorda) -> ispisuje stringovnu vrednost rekorda u fajl


-- funkcija za parsiranje dokumenta
parseDocument :: Parsec String Int Document -- prima Parsec (parser) kao prvi argument, drugi argument je sadrzaj fajla a treci je povratna vrednost tj Dokument tip
parseDocument = do
  records <- many parseRecord-- many parsira 0 ili vise puta funkciju parseRecords, rezultat je niz rekorda
  return (Document records) -- vraca dokument sa rekordima


-- funkcija za parsiranje rekorda
parseRecord :: Parsec String Int Record -- potpis je slican kao od funckije iznad ^
parseRecord = do
  leader <- try parseLeader <|> return (Leader "" "") -- pokusava da isparsira leader a ako to ne uspe vrati prazan leader
  recordMARCType <- getState -- vadi state iz parsera, za slucaj da je uspelo parsiranje leader-a
  if((recordMARCType:: Int) == 1) then do -- proverava tip MARC formata
    fields <- many parseFieldType1 -- ukoliko je prvi tip, onda parsira field-ove za taj odgovarajuci tip
    char groupSeparator -- jede GS na kraju record-a da bi se mogao parsirati sledeci potencijalni record
    return (Record leader fields) -- vraca record sa leader-om i field-ovima
  else if((recordMARCType:: Int) == 2) then do -- drugi tip
      fields <- many parseFieldType2 -- parsira polja za drugi tip
      char groupSeparator
      return (Record leader fields)
      else if((recordMARCType:: Int) == 3) then do
        fields <- many parseFieldType3
        char groupSeparator
        return (Record leader fields)
          else do
            fields <- many parseFieldType4And5
            char groupSeparator
            return (Record leader fields)


-- funkcija za parsiranje leader-a
parseLeader :: Parsec String Int Leader
parseLeader = do
  code <- try (string "000") <|> try (string "LEADER") <|> try (string "LDR") -- pokusamo da parsiramo jedan od tri vrste leader-a, ako ne uspe ova funkcija se zavrsava
  case code of -- switch nad vrednosti koju smo isparsirali
    "000" -> putState 2 -- u pitanju je treci tip marc-a, stavlja ga u state parsera
    "LEADER" -> putState 3 -- drugi tip, ditto
    "LDR" -> putState 1 -- prvi, ditto
  spaces -- jede space-ove, ukoliko ih ima
  value <- many(noneOf [carriageReturn, newLine]) -- parsira sve karaktere dok ne dodje do CRLF karaktera
  try (char carriageReturn) <|> try (char newLine) -- jede CRLF karakter
  return (Leader code value) -- vraca leader sa kodom i vrednoscu


-- FUNKCIJE ZA PARSIRANJE PRVOG TIPA MARC FORMATA

-- funkcija za parsiranje field-a za prvi tip
parseFieldType1 :: Parsec String Int Field
parseFieldType1 = do
  code <- count 3 digit -- parsira prve tri cifre, one predstavljaju kod field-a
  spaces -- jede space-ove ukoliko ih ima
  if((read code:: Int) < 10) then do -- field je kontrolnog tipa (svi manji od 010 su kontrolni), (read code:: Int) pretvara string u int
    value <- many(noneOf [rowSeparator]) -- vrednost field-a je sve dok se ne dodje do RS karaktera
    char rowSeparator -- jede RS karakter
    return (Field code [] [(Subfield "" value)]) -- vraca se field sa svojim kodom, praznim indikatorima i vrednoscu koja je predstavljena kroz potpolje sa praznim kodom i parsiranom vrednoscu
  else do -- field nije kontrolni, parsira i indikatore
    indicators <- parseIndicators -- parsira indikatore
    subfields <- many parseSubfieldType1 -- parsira subfield-ove
    char rowSeparator -- jede RS karakter koji se nalazi na kraju field-a da bi mogao uspesno da se parsira sledeci field ukoliko postoji
    return (Field code indicators subfields) -- vraca field sa svojim kodom, indikatorima i potpoljima


-- funkcija za parsiranje indikatora
parseIndicators :: Parsec String Int [Indicator] -- slicno kao i prethodne funkcije, samo sto vraca listu indikatora
parseIndicators = do
  firstIndicator <- anyChar -- parsira bilo koji karakter
  secondIndicator <- anyChar -- ditto
  return [(Indicator firstIndicator), (Indicator secondIndicator)]


-- funkcija za parsiranje subfield-ova
parseSubfieldType1 :: Parsec String Int Subfield
parseSubfieldType1 = do
   spaces -- jede moguce space-ove
   char '$' -- jede oznaku za pocetak subfield-a
   subfieldName <- anyChar -- parsira karakter koji predstavlja naziv subfield-a
   spaces -- jede moguce space-ove
   subfieldValue <- many(noneOf ['$', rowSeparator]) -- parsira vrednost subfield-a, jeduci sve karaktere dok ne dodje do sledece oznake za subfield ili do kraja field-a
   return (Subfield [subfieldName] subfieldValue) -- vraca subfield sa imenom (string duzine 1 karaktera) i vrednoscu


-- FUNKCIJE ZA PARSIRANJE DRUGOG TIPA MARC FORMATA

-- funkcija za parsiranje fieldova za tip 2
parseFieldType2 :: Parsec String Int Field
parseFieldType2 = do
  code <- count 3 digit -- parsira prve tri cifre, one predstavljaju kod field-a
  spaces -- jede space-ove ukoliko ih ima
  if((read code:: Int) < 10) then do -- field je kontrolnog tipa (svi manji od 010 su kontrolni), (read code:: Int) pretvara string u int
   value <- many(noneOf [rowSeparator]) -- vrednost field-a je sve dok se ne dodje do RS karaktera
   char rowSeparator -- jede RS karakter
   return (Field code [] [(Subfield "" value)]) -- vraca se field sa svojim kodom, praznim indikatorima i vrednoscu koja je predstavljena kroz potpolje sa praznim kodom i parsiranom vrednoscu
  else do -- field nije kontrolni, parsira i indikatore
   indicators <- parseIndicators -- parsira indikatore
   subfields <- many parseSubfieldType2 -- parsira subfield-ove
   char rowSeparator -- jede RS karakter koji se nalazi na kraju field-a da bi mogao uspesno da se parsira sledeci field ukoliko postoji
   return (Field code indicators subfields) -- vraca field sa svojim kodom, indikatorima i potpoljima

-- funkcija za parsiranje subfield-ova
parseSubfieldType2 :: Parsec String Int Subfield
parseSubfieldType2 = do
  spaces -- jede moguce space-ove
  char '|' -- jede oznaku za pocetak subfield-a
  subfieldName <- anyChar -- parsira karakter koji predstavlja naziv subfield-a
  spaces -- jede moguce space-ove
  subfieldValue <- many(noneOf ['|', rowSeparator]) -- parsira vrednost subfield-a, jeduci sve karaktere dok ne dodje do sledece oznake za subfield ili do kraja field-a
  return (Subfield [subfieldName] subfieldValue) -- vraca subfield sa imenom (string duzine 1 karaktera) i vrednoscu


-- FUNKCIJE ZA PARSIRANJE TRECEG TIPA MARC FORMATA
-- funkcija za parsiranje field-a za treci tip
parseFieldType3 :: Parsec String Int Field
parseFieldType3 = do
  code <- count 3 digit -- parsira prve tri cifre, one predstavljaju kod field-a
  spaces -- jede space-ove ukoliko ih ima
  if((read code:: Int) < 10) then do -- field je kontrolnog tipa (svi manji od 010 su kontrolni), (read code:: Int) pretvara string u int
    value <- many(noneOf [rowSeparator]) -- vrednost field-a je sve dok se ne dodje do RS karaktera
    char rowSeparator -- jede RS karakter
    return (Field code [] [(Subfield "" value)]) -- vraca se field sa svojim kodom, praznim indikatorima i vrednoscu koja je predstavljena kroz potpolje sa praznim kodom i parsiranom vrednoscu
  else do -- field nije kontrolni, parsira i indikatore
    indicators <- parseIndicatorsType3 -- parsira indikatore
    subfields <- many parseSubfieldType1 -- parsira subfield-ove
    char rowSeparator -- jede RS karakter koji se nalazi na kraju field-a da bi mogao uspesno da se parsira sledeci field ukoliko postoji
    return (Field code indicators subfields) -- vraca field sa svojim kodom, indikatorima i potpoljima


-- funkcija za parsiranje indikatora
parseIndicatorsType3 :: Parsec String Int [Indicator] -- slicno kao i prethodne funkcije, samo sto vraca listu indikatora
parseIndicatorsType3 = do
  charAhead <- lookAhead anyChar
  if(charAhead == '$') then do
    return []
  else do
    firstIndicator <- anyChar -- parsira bilo koji karakter
    secondIndicator <- anyChar -- ditto
    return [(Indicator firstIndicator), (Indicator secondIndicator)]


-- FUNKCIJE ZA PARSIRANJE CETVRTOG I PETOG TIPA MARC FORMATA
parseFieldType4And5 :: Parsec String Int Field
parseFieldType4And5 = do
  code <- count 3 digit -- parsira prve tri cifre, one predstavljaju kod field-a
  indicators <- parseIndicatorsType4And5 -- parsira indikatore
  if((length indicators) > 0) then do
    subfields <- many parseSubfieldType4
    char rowSeparator
    return (Field code indicators subfields)
    else do
    subfields <- many parseSubfieldType5 -- parsira subfield-ove
    char rowSeparator -- jede RS karakter koji se nalazi na kraju field-a da bi mogao uspesno da se parsira sledeci field ukoliko postoji
    return (Field code indicators subfields) -- vraca field sa svojim kodom, indikatorima i potpoljima


-- funkcija za parsiranje indikatora
parseIndicatorsType4And5 :: Parsec String Int [Indicator] -- slicno kao i prethodne funkcije, samo sto vraca listu indikatora
parseIndicatorsType4And5 = do
  spaces
  possibleUnitSeparator <- lookAhead anyChar
  if(possibleUnitSeparator == unitSeparator) then do
    return []
  else do
    firstIndicator <- anyChar -- parsira bilo koji karakter
    secondIndicator <- anyChar -- ditto
    spaces
    return [(Indicator firstIndicator), (Indicator secondIndicator)]


parseSubfieldType4 :: Parsec String Int Subfield
parseSubfieldType4 = do
  try (char '[') -- jede oznaku za pocetak naziva subfield-a
  subfieldName <- anyChar -- parsira karakter koji predstavlja naziv subfield-a
  char ']' -- jede oznaku za kraj naziva subfield-a
  spaces -- jede moguce space-ove
  subfieldValue <- many(noneOf ['[', rowSeparator]) -- parsira vrednost subfield-a, jeduci sve karaktere dok ne dodje do sledece oznake za subfield ili do kraja field-a
  return (Subfield [subfieldName] subfieldValue) -- vraca subfield sa imenom (string duzine 1 karaktera) i vrednoscu


parseSubfieldType5 :: Parsec String Int Subfield
parseSubfieldType5 = do
  char unitSeparator -- jede oznaku za pocetak naziva subfield-a
  subfieldName <- anyChar -- parsira karakter koji predstavlja naziv subfield-a
  spaces -- jede moguce space-ove
  subfieldValue <- many(noneOf [unitSeparator, rowSeparator]) -- parsira vrednost subfield-a, jeduci sve karaktere dok ne dodje do sledece oznake za subfield ili do kraja field-a
  return (Subfield [subfieldName] subfieldValue) -- vraca subfield sa imenom (string duzine 1 karaktera) i vrednoscu




-- show funkcije
instance Show Document where
    show (Document records) =
                        "{\n" ++
                        "\"records\":\n" ++ (show records) ++
                        "\n}"

instance Show Record where
    show (Record leader fields) =
                        "\n" ++ insertTabs 1 ++ "{\n" ++
                        insertTabs 2 ++ "\"leader\": " ++ (show leader) ++ "\n" ++
                        insertTabs 2 ++ "\"fields\":\n" ++
                        insertTabs 2 ++ "[ \n" ++
                        showFieldHelper fields ++
                        "\n" ++ insertTabs 2 ++ "]" ++
                        "\n" ++ insertTabs 1 ++ "}\n"

instance Show Leader where
    show (Leader code value) = "\"" ++ value ++ "\""

showFieldHelper :: [Field] -> String
showFieldHelper [] = ""
showFieldHelper (field:restOfFields) =
    (show field) ++ ",\n" ++ showFieldHelper restOfFields


instance Show Field where
    show (Field code indicators subfields) =
      insertTabs 3 ++ "{\n" ++
      insertTabs 4 ++ show code ++ ":\n" ++
      insertTabs 4 ++ "{\n" ++
      insertTabs 5 ++ "\"subfields\":\n" ++
      insertTabs 5 ++ "[\n" ++
      showSubFieldHelper subfields ++
      insertTabs 5 ++ "],\n" ++
      insertTabs 5 ++ (printIndicators indicators) ++ "\n" ++
      insertTabs 4 ++ "}\n" ++
      insertTabs 3 ++ "}"


instance Show Indicator where
  show (Indicator code) = show code

instance Show Subfield where
    show (Subfield subfieldCode subfieldContent) =
      insertTabs 6 ++ "{\n" ++
      insertTabs 7 ++ "\"" ++ subfieldCode ++ "\":" ++ "\"" ++ subfieldContent ++ "\"" ++ "\n" ++
      insertTabs 6 ++ "}"


printIndicators :: [Indicator] -> String
printIndicators [] = ""
printIndicators (x:y:[]) = "\"indicator_1\": " ++ (show x) ++ ",\n" ++ insertTabs 5 ++ "\"indicator_2:\" " ++ (show y)

-- kako resiti trailing zarez, pattern matchuj da li je poslednji, ako jeste stavi bez zarreza
showSubFieldHelper :: [Subfield] -> String
showSubFieldHelper [] = ""
showSubFieldHelper (subfield:restOfSubFields) =
                        (show subfield) ++ ",\n" ++ showSubFieldHelper restOfSubFields

insertTabs :: Int -> String
insertTabs 0 = ""
insertTabs c = "\t" ++ insertTabs (c - 1)
