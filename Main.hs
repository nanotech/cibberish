import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.List (intersperse)
import Char (isAlpha)
import Test.QuickCheck (Gen, Arbitrary, arbitrary, elements, frequency,
                        oneof, suchThat, resize, listOf1, sample')

data CSizeModifier = Short  | Long      | DefaultSize deriving (Eq)
data CSignModifier = Signed | Unsigned  | DefaultSign deriving (Eq)
data CQualifier    = Cconst | Cvolatile | NoQualifier deriving (Eq)
data CStorageClass = Extern | Static    | Auto        deriving (Eq)

data CNumType = Cint CSignModifier CSizeModifier
              | Cchar CSignModifier
              | Cfloat | Cdouble
  deriving (Eq)

type CQualifiers = (CQualifier,CQualifier)

data CType = CType CQualifiers CNumType
           | CPointer CQualifiers CType
           | CFunction CType [CType]
  deriving (Eq)

newtype Identifier = Identifier String
  deriving (Eq)

data Declaration = Declaration CStorageClass CType Identifier
  deriving (Eq)

instance Arbitrary CSizeModifier where
  arbitrary = elements [Short, Long, DefaultSize]

instance Arbitrary CSignModifier where
  arbitrary = elements [Signed, Unsigned, DefaultSign]

instance Arbitrary CQualifier where
  arbitrary = frequency [(2, return Cconst)
                        ,(2, return Cvolatile)
                        ,(8, return NoQualifier)
                        ]

instance Arbitrary CStorageClass where
  arbitrary = frequency [(5,  return Extern)
                        ,(5,  return Static)
                        ,(30, return Auto)
                        ]

instance Arbitrary CNumType where
  arbitrary = oneof [ liftM2 Cint arbitrary arbitrary
                    , liftM Cchar arbitrary
                    , elements [Cfloat, Cdouble]
                    ]

instance Arbitrary CType where
  arbitrary = frequency [(5, liftM2 CType qual arbitrary)
                        ,(3, liftM2 CPointer qual arbitrary)
                        ,(2, liftM2 CPointer qual (liftM2 CFunction arbitrary param))
                        ]
    where qual = suchThat arbitrary (\(a,b) -> a /= b || (a == b && a == NoQualifier))
          param = resize 2 arbitrary

instance Arbitrary Identifier where
  arbitrary = do str <- listOf1 $ elements ('_' : ['a'..'z'])
                 return (Identifier str)

instance Arbitrary Declaration where
  arbitrary = liftM3 Declaration arbitrary arbitrary arbitrary

join :: String -> [String] -> String
join sep l = concat (intersperse sep (filter (not.null) l))

sjoin :: [String] -> String
sjoin l = concat (intersperse " " (filter (not.null) l))

smush :: [String] -> String
smush l = concat (filter (not.null) l)

showSpace a
  | res == "" = ""
  | otherwise = res ++ " "
  where res = show a

instance Show CNumType where
  show (Cint s z) = sjoin [show s, show z, "int"]
  show (Cchar s) = sjoin [show s, "char"]
  show Cfloat = "float"
  show Cdouble = "double"

instance Show CSizeModifier where
  show Short = "short"
  show Long = "long"
  show DefaultSize = ""

instance Show CSignModifier where
  show Signed = "signed"
  show Unsigned = "unsigned"
  show DefaultSign = ""

instance Show CQualifier where
  show Cconst = "const"
  show Cvolatile = "volatile"
  show NoQualifier = ""

instance Show CStorageClass where
  show Extern = "extern"
  show Static = "static"
  show Auto = ""

instance Show CType where
  show (CType q t) = sjoin [showQual q, show t]

instance Show Identifier where
  show (Identifier s) = s

instance Show Declaration where
  show (Declaration s t n) = sjoin [show s, fancyShow (show n) t]

class Sideful a where
  sides :: Bool -> a -> (String,String)
  subnode :: a -> Maybe a

sidesof :: Sideful a => a -> (String,String)
sidesof sideful = sof True (Just sideful) [] []
  where sof root (Just w) l r = sof False (subnode w) (x:l) (y:r)
          where (x,y) = sides root w
        sof _ Nothing l r = (concat l, concat $ reverse r)

fancyShow m s = l ++ lsp ++ m ++ rsp ++ r
  where (l,r) = sidesof s
        nlsp = nn m && nn l && (isAlpha $ last l) && (isAlpha $ head m)
        nrsp = nn m && nn r && (isAlpha $ head r)
        lsp = if nlsp then " " else ""
        rsp = if nrsp then " " else ""
        nn = not . null

showQual (a,b) = sjoin [show a, show b]

instance Sideful CType where
  sides _ t@(CType _ _) = (show t, "")
  sides _ t@(CPointer q _) = ("*"++showQual q, "")
  sides root t@(CFunction _ args) = (l, r++ join ", " (map (fancyShow "") args) ++")")
    where l = if root then "" else "("
          r = if root then "" else ")("
  subnode (CType _ _) = Nothing
  subnode (CPointer _ t) = Just t
  subnode (CFunction t _) = Just t

noQualifiers = (NoQualifier, NoQualifier)
setQual f x =
  case x of
    (CType q n) -> (CType (f q) n)
    (CPointer q t) -> (CPointer (f q) t)
    (CFunction _ _) -> error "Functions cannot have qualifiers."
cconst = setQual (\(a,b) -> (Cconst,b))
volatile = setQual (\(a,b) -> (a,Cvolatile))

float = (CType noQualifiers Cfloat)
double = (CType noQualifiers Cdouble)
char = (CType noQualifiers (Cchar DefaultSign))
int = (CType noQualifiers (Cint DefaultSign DefaultSize))
short (CType q (Cint sign _)) = (CType q (Cint sign Short))
long (CType q (Cint sign _)) = (CType q (Cint sign Long))

setSign sign (CType q (Cint _ size)) = (CType q (Cint sign size))
setSign sign (CType q (Cchar _)) = (CType q (Cchar sign))

unsigned = setSign Unsigned
signed = setSign Signed

function = CFunction
pointer = CPointer noQualifiers
funcPointer = function . pointer

main = do
  samples <- sample' $ resize 20 (arbitrary :: Gen Declaration)
  putStrLn "int main(int argc, char const* argv[])\n{"
  putStrLn $ join "\n" $ map (\x -> "\t{ " ++ show x ++ "; }") samples
  putStrLn "\treturn 0;\n}"
  --print $ Declaration Auto (function (pointer (function int [])) []) (Identifier "foo")
  --print $ Declaration Auto (pointer (function int [])) (Identifier "foo")
  --print $ Declaration Static (volatile $ cconst $ pointer $ function (pointer (function (pointer $ unsigned int) [])) [(pointer (function double [])), float]) (Identifier "foo")
  --print $ Declaration Static (pointer $ pointer $ function (signed int) []) (Identifier "foo")
  --print $ Declaration Auto (function (cconst int) []) (Identifier "foo")
