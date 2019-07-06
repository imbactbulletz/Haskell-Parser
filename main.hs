-- biblioteke za rad sa parserom i fajlovima
import Text.Parsec
import System.Environment


-- gramatika
data Document = Document [Record] deriving (Show)

data Record = Record Leader [Field] deriving (Show)

data Leader = Leader Code Value deriving (Show)

data Field = Field Code [Indicator] [Subfield] deriving (Show)

data Indicator = Indicator Char deriving (Show)

data Subfield = Subfield Code Value deriving (Show)

type Value = String

type Code = String

-- shorthand za UNIMARC nevidljive znakove
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
  else do -- ostali slucajevi TODO
    char groupSeparator
    return (Record leader [])


-- funkcija za parsiranje leader-a
parseLeader :: Parsec String Int Leader
parseLeader = do
  code <- try (string "000") <|> try (string "LEADER") <|> try (string "LDR") -- pokusamo da parsiramo jedan od tri vrste leader-a, ako ne uspe ova funkcija se zavrsava
  case code of -- switch nad vrednosti koju smo isparsirali
    "000" -> putState 3 -- u pitanju je treci tip marc-a, stavlja ga u state parsera
    "LEADER" -> putState 2 -- drugi tip, ditto
    "LDR" -> putState 1 -- prvi, ditto
  spaces -- jede space-ove, ukoliko ih ima
  value <- many(noneOf [carriageReturn, newLine]) -- parsira sve karaktere dok ne dodje do CRLF karaktera
  try (char carriageReturn) <|> try (char newLine) -- jede CRLF karakter
  return (Leader code value) -- vraca leader sa kodom i vrednoscu


-- FUNKCIJE ZA PARSIRANJE PRVOG TIPA MARC FORMATA

-- funkcija za parsiranje field-a za prvi tip
parseFieldType1 :: Parsec String Int Field
parseFieldType1 = do
  code <- count 3 digit -- parsira prve tri cifre, one predstasvljaju kod field-a
  spaces -- jede space-ove ukolikoh ih ima
  if((read code:: Int) < 10) then do -- field je kontrolnog tipa (svi manji od 010 su kontrolni), (read code:: Int) pretvara string u int
    value <- many(noneOf [rowSeparator]) -- vrednost field-a je sve dok se ne dodje do RS karaktera
    char rowSeparator -- jede RS karakter
    return (Field code [] [(Subfield "" value)]) -- vraca se field sa svojim kodom, praznim indikatorima i vrednoscu koja je predstavljena kroz potpolje sa praznim kodom i parsiranom vrednoscu
  else do -- field nije kontrolni, parsira i indikatore
    indicators <- parseIndicatorsType1 -- parsira indikatore
    subfields <- many parseSubfieldType1 -- parsira subfield-ove
    char rowSeparator -- jede RS karakter koji se nalazi na kraju field-a da bi mogao uspesno da se parsira sledeci field ukoliko postoji
    return (Field code indicators subfields) -- vraca field sa svojim kodom, indikatorima i potpoljima


-- funkcija za parsiranje indikatora
parseIndicatorsType1 :: Parsec String Int [Indicator] -- slicno kao i prethodne funkcije, samo sto vraca listu indikatora
parseIndicatorsType1 = do
  firstIndicator <- anyChar -- parsira bilo koji karakter
  secondIndicator <- anyChar -- ditto
  return [(Indicator firstIndicator), (Indicator secondIndicator)]


-- funkcija za parsiranje subfield-ova
parseSubfieldType1 :: Parsec String Int Subfield
parseSubfieldType1 = do
   spaces -- jede moguce space-ove
   char '$' -- jede oznaku za pocetak subfield-a
   subfieldName <- anyChar -- parsira karakter koji predstavlja naziv subfield-a
   subfieldValue <- many(noneOf ['$', rowSeparator]) -- parsira vrednost subfield-a, jeduci sve karaktere dok ne dodje do sledece oznake za subfield ili do kraja field-a
   return (Subfield [subfieldName] subfieldValue) -- vraca subfield sa imenom (string duzine 1 karaktera) i vrdnoscu
