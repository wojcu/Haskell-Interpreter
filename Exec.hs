module Exec (Loc
 , TypeName
 , Type(..)
 , Scope
 , outs
 , empty
 , popLocation
 , builtinTypes
 , getNameMap
 , setNameMap
 , cloneLocation
 , tieLocName
 , checkLocType
 , checkLocExists
 , addInt
 , addProcedure
 , addFunction
 , addDouble
 , addChar
 , addBool
 , addPointer
 , addString
 , addArray
 , addUserDef
 , withRetVal
 , addFromType
 , hasRetVal
 , fetchFunction
 , fetchProcedure
 , fetchString
 , fetchChar
 , fetchDouble
 , fetchInt
 , fetchPointer
 , fetchArray
 , fetchBool
 , keywords
 , loadBuiltins
 , getFromName
 , checkNameBound
 , getLocationType
 , addNull
 , getRetVal
 , clearRetVal
 , runMain
 , setLocType
 , pushLayer
 , popLayer
 , getLocMember
) where 

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ErrM
import Debug.Trace
import Data.Char

type Loc = Int
type TypeName = String

data Type = UserType String | Builtin String | Pointer Type | Array Type Int | Lambda Type [Type] | LambdaVoid [Type] | Null deriving (Show, Eq)

type NameHistory = (String, Loc, Maybe Loc)
type Layer = [NameHistory]

data Scope = Scope { nextLocation :: Loc
 , locationsUsed :: Loc
 , unusedLoc :: [Loc]
 , nameToLoc :: Map.Map String Loc
 , locType :: Map.Map Loc Type
 , userObjects :: Map.Map Loc (Map.Map String Loc)
 , ints :: Map.Map Loc Int
 , strings :: Map.Map Loc String
 , chars :: Map.Map Loc Char
 , bools :: Map.Map Loc Bool
 , doubles :: Map.Map Loc Double
 , arrays :: Map.Map Loc [Loc]
 , pointers :: Map.Map Loc Loc
 , userDefs :: Map.Map String (Map.Map String Type)
 , functions :: Map.Map Loc ((Scope -> [Loc] -> Err (Scope, Loc)), Map.Map String Loc)
 , procedures :: Map.Map Loc ((Scope -> [Loc] -> Err Scope), Map.Map String Loc)
 , nulls :: Set.Set Loc
 , retVal :: Maybe Loc
 , layers :: [Layer]
 , outs :: [String]
}

instance Show Scope where
 show s@(Scope{}) = "Scope (\n" ++ "\tnext: " ++ (show (nextLocation s)) ++ "\n\tused: " ++ (show (locationsUsed s)) ++ "\n\tunused: " ++ (show (unusedLoc s)) ++ "\n\tnames: " ++ (show (nameToLoc s)) ++ "\n\tlayers: " ++ (show (layers s)) ++ "\n\ttypes: " ++ (show (locType s)) ++ "\n)"

empty = Ok (Scope 1 1 [] (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Map.empty) (Set.empty) Nothing ([[]]) [])

getNameMap (Scope {nameToLoc = nm}) = Ok nm

setNameMap nm s = Ok (s {nameToLoc = nm})

pushLayer s@(Scope {layers = ls}) = Ok (s {layers = ([]):ls})

pushNameVersion ver s@(Scope {layers = (ly:lys)}) = Ok (s {layers = (ver:ly):lys})

restoreLayer ((nm, _, Just l):lys) s@(Scope {nameToLoc = nl}) = 
  do
    checkLocExists l s
    s <- Ok (s {nameToLoc = Map.insert nm l nl})
    restoreLayer lys s

restoreLayer ((nm, _, Nothing):lys) s@(Scope {nameToLoc = nl}) =
  do
    s <- Ok(s {nameToLoc = Map.delete nm nl})
    restoreLayer lys s

restoreLayer [] s = Ok s

popLayer s@(Scope {layers = (ly:lys)}) = restoreLayer ly (s {layers = lys})

popLocation s@(Scope {unusedLoc = (x:xs)}) = Ok ((s {unusedLoc = xs, nextLocation = x}, (nextLocation s)))

popLocation s@(Scope {locationsUsed = v, unusedLoc = []}) = Ok ((s {nextLocation = (v + 1), locationsUsed = (v + 1)}, (nextLocation s)))

loadBuiltins s = do
  s <- _loadBuiltinProcedures builtinProcedures s
  s <- _loadBuiltinFunctions builtinFunctions s
  Ok s
  where
   _loadBuiltinProcedures (p:ps) s = 
    do
      (s, l) <- popLocation s
      s <- tieLocName l fname s
      nmap <- getNameMap s
      s <- addProcedure l ftype fproc nmap s
      _loadBuiltinProcedures ps s where (fname, ftype, fproc) = p
   _loadBuiltinProcedures [] s = Ok s
   _loadBuiltinFunctions (f:fs) s = 
    do
      (s, l) <- popLocation s
      s <- tieLocName l fname s
      nmap <- getNameMap s
      s <- addFunction l ftype fproc nmap s
      _loadBuiltinFunctions fs s where (fname, ftype, fproc) = f
   _loadBuiltinFunctions [] s = Ok s

getLocationType l s = 
  do 
    l <- checkLocExists l s
    Ok ((locType s) Map.! l)  

getFromName n s =
  do
    n <- checkNameBound n s
    Ok ((nameToLoc s) Map.! n)

checkLocExists l s = if Map.member l (locType s) then Ok l else Bad "Location does not exist."

checkLocType l t s = do lt <- getLocationType l s
                        if lt == t then Ok l else Bad ("Location type mismatch: " ++ (show l) ++ " is not a " ++ (show t) ++ " in " ++ (show s))

checkNameBound n s = if Map.member n (nameToLoc s) then Ok n else Bad ("Name \"" ++ n ++ "\" not bound.")

hasRetVal (Scope {retVal = Nothing}) = False

hasRetVal (Scope {retVal = Just _}) = True

getRetVal (Scope {retVal = Nothing}) = Bad "No return value."

getRetVal (Scope {retVal = Just l}) = Ok l

getLocMember l nm s =
  do
    fm <- fetchUserObject l s
    res <- if Map.member nm fm then Ok (fm Map.! nm) else Bad "Bad member name for type."
    Ok (s, res)


fetchProcedure l s = 
  do
    lt <- getLocationType l s
    _fetchProcedure l lt s 
    where
      _fetchProcedure l (LambdaVoid _) s = Ok ((procedures s) Map.! l)
      _fetchProcedure _ _ _ = Bad "Not a procedure."

fetchFunction l s = 
  do
    lt <- getLocationType l s
    _fetchFunction l lt s 
    where
      _fetchFunction l (Lambda _ _) s = Ok ((functions s) Map.! l)
      _fetchFunction _ _ _ = Bad "Not a function."

fetchString l s = 
  do
    lt <- getLocationType l s
    _fetchString l lt s 
    where
      _fetchString l (Builtin args) s = if args == "string" then Ok ((strings s) Map.! l) else (Bad "Not a string.")
      _fetchString _ _ _ = Bad "Not a string."

fetchInt l s = 
  do
    lt <- getLocationType l s
    _fetchInt l lt s 
    where
      _fetchInt l (Builtin args) s = if args == "int" then Ok ((ints s) Map.! l) else (Bad "Not an int.")
      _fetchInt _ _ _ = Bad "Not an int."

fetchBool l s = 
  do
    lt <- getLocationType l s
    _fetchBool l lt s 
    where
      _fetchBool l (Builtin args) s = if args == "bool" then Ok ((bools s) Map.! l) else (Bad "Not a bool.")
      _fetchBool _ _ _ = Bad "Not a bool."

fetchChar l s =
  do
    lt <- getLocationType l s
    _fetchChar l lt s
    where
      _fetchChar l (Builtin args) s = if args == "char" then Ok ((chars s) Map.! l) else (Bad "Not a char.")
      _fetchChar _ _ _ = Bad "Not a char."

fetchDouble l s =
  do
    lt <- getLocationType l s
    _fetchDouble l lt s
    where
      _fetchDouble l (Builtin args) s = if args == "double" then Ok ((doubles s) Map.! l) else (Bad "Not a double.")
      _fetchDouble _ _ _ = Bad "Not a double."

fetchPointer l s =
  do
    lt <- getLocationType l s
    _fetchPointer l lt s
    where
      _fetchPointer l (Pointer _) s = Ok ((pointers s) Map.! l)
      _fetchPointer _ _ _ = Bad "Not a pointer."

fetchUserObject l s =
  do
    lt <- getLocationType l s
    tn <- case lt of (UserType tn) -> Ok tn
                     _ -> Bad "Not a user object."
    Ok ((userObjects s) Map.! l)

fetchArray l s =
  do
    lt <- getLocationType l s
    _fetchArray l lt s
    where
      _fetchArray l (Array _ _) s = Ok ((arrays s) Map.! l)
      _fetchArray _ _ _ = Bad "Not an array."



setLocType l t s@(Scope{locType = lt}) = Ok (s {locType = Map.insert l t lt})

addInt l v s@(Scope{locType = ts, ints = vs}) = Ok (s {locType = Map.insert l (Builtin "int") ts, ints = Map.insert l v vs})

addBool l v s@(Scope{locType = ts, bools = vs}) = Ok (s {locType = Map.insert l (Builtin "bool") ts, bools = Map.insert l v vs})

addString l v s@(Scope{locType = ts, strings = vs}) = Ok (s {locType = Map.insert l (Builtin "string") ts, strings = Map.insert l v vs})

addChar l v s@(Scope{locType = ts, chars = vs}) = Ok (s {locType = Map.insert l (Builtin "char") ts, chars = Map.insert l v vs})

addDouble l v s@(Scope{locType = ts, doubles = vs}) = Ok (s {locType = Map.insert l (Builtin "double") ts, doubles = Map.insert l v vs})

addProcedure l t f nmap s@(Scope{locType = ts, procedures = fs}) = Ok (s {locType = Map.insert l t ts, procedures = Map.insert l (f, nmap) fs})

addFunction l t f nmap s@(Scope{locType = ts, functions = fs}) = Ok (s {locType = Map.insert l t ts, functions = Map.insert l (f, nmap) fs})

addNull l s@(Scope{locType = ts, nulls = vs}) = Ok (s {locType = Map.insert l Null ts, nulls = Set.insert l vs})

addPointer l t s@(Scope{locType = ts, pointers = fs}) = 
  do
    ttype <- getLocationType t s
    Ok (s {locType = Map.insert l (Pointer ttype) ts, pointers = Map.insert l t fs})

buildArray t size s = _buildArray t size [] s where
  _buildArray t 0 a s = Ok (s, a)
  _buildArray t size a s =
    do
      (s, l) <- popLocation s
      s <- addFromType l t s
      _buildArray t (size - 1) (a ++ [l]) s

addArray l t size s =  
  do
    (s, a) <- buildArray t size s
    Ok (s {locType = Map.insert l (Array t size) (locType s), arrays = Map.insert l a (arrays s)})

addUserDef nm fields s@(Scope {userDefs = ud}) = Ok (s {userDefs = Map.insert nm fields ud})

tieLocName l n s@(Scope{nameToLoc = nls}) = 
  do
    hist <- if Map.member n nls then Ok (n, l, Just (nls Map.! n)) else Ok (n, l, Nothing)
    s <- pushNameVersion hist s 
    Ok (s {nameToLoc = Map.insert n l nls})


checkTypeKnown nm s@(Scope {userDefs = ud}) = if Map.member nm ud then Ok (nm) else Bad "Type unknown." 

getTypeFields nm s@(Scope {userDefs = ud}) =
  do 
    checkTypeKnown nm s
    Ok (ud Map.! nm)

addUserObject l nm s =
  do
    fm <- getTypeFields nm s
    (lm, s) <- Map.foldrWithKey genOneLoc (Ok (Map.empty, s)) fm
    Ok (s {userObjects = Map.insert l lm (userObjects s), locType = (Map.insert l (UserType nm) (locType s))})
    where
      genOneLoc k v (Ok (a, s)) =
        do
          (s, l) <- popLocation s
          s <- addFromType l v s
          Ok ((Map.insert k l a), s)
      genOneLoc _ _ (Bad s) = Bad s

addFromType l (Builtin b) s  | b == "int" = addInt l 0 s
                           | b == "bool" = addBool l False s
                           | b == "char" = addChar l '\0' s
                           | b == "string" = addString l "" s
                           | b == "double" = addDouble l 0.0 s

addFromType l (UserType nm) s = addUserObject l nm s


addFromType l (Pointer t) s =
  do
    (s, sl) <- popLocation s
    s <- addFromType sl t s
    addPointer l sl s

addFromType l (Null) s = addNull l s

addFromType l t@(LambdaVoid _) s = 
  do
    nmap <- getNameMap s
    addProcedure l t (\s _ -> Ok s) nmap s

addFromType l t@(Lambda rt argtypes) s = 
  do
    nmap <- getNameMap s
    addFunction l t body nmap s
    where 
    body scope arglocs = 
      do
        scope <- if (map Ok argtypes) == (map (\v -> getLocationType v scope) arglocs) then Ok scope else Bad "Argument types mismatch."
        (scope, nl) <- popLocation scope
        scope <- addFromType nl rt scope
        Ok (scope, nl)

addFromType l (Array t c) s = addArray l t c s

withRetVal l s = Ok (s {retVal = Just l})

clearRetVal s = Ok (s {retVal = Nothing})

cloneLocation l nl s@(Scope {
    locType = lt,
    userObjects = uo,
    ints = is,
    strings = ss,
    chars = cs,
    bools = bs,
    doubles = ds,
    arrays = as,
    pointers = pts,
    functions = fs,
    procedures = ps,
    nulls = ns
    }) = Ok s{
      locType = cloneIfMember l lt nl,
      userObjects = cloneIfMember l uo nl,
      ints = cloneIfMember l is nl,
      strings = cloneIfMember l ss nl,
      chars = cloneIfMember l cs nl,
      bools = cloneIfMember l bs nl,
      doubles = cloneIfMember l ds nl,
      arrays = cloneIfMember l as nl,
      pointers = cloneIfMember l pts nl,
      functions = cloneIfMember l fs nl,
      procedures = cloneIfMember l ps nl,
      nulls = if Set.member l ns then Set.insert nl ns else ns
    }
  where 
    cloneIfMember k m n = if Map.member k m then Map.insert n (m Map.! k) m else m
    
runMain s = 
  do
    l <- getFromName "main" s
    (proc, _) <- fetchProcedure l s
    proc s []


builtinTypes = ["int", "char", "string", "double", "bool"]

keywords = ["struct", "return", "while", "if"]

builtinProcedures = [("print", LambdaVoid [Builtin "string"], _builtin_print), ("crash", LambdaVoid [Builtin "string"], _builtin_crash)]

builtinFunctions = [("int_to_char", Lambda (Builtin "int") [Builtin "char"]
 , _builtin_char_to_int), ("int_to_string", Lambda (Builtin "string") [Builtin "int"], _builtin_int_to_string)]

_builtin_char_to_int scope args = 
  do 
    l <- if length args == 1 then Ok (head args) else Bad "Too many argument for int_to_char(char)."
    checkLocType l (Builtin "char") scope
    (scope, nl) <- popLocation scope
    chr <- fetchChar l scope
    scope <- addInt nl (Data.Char.ord chr) scope
    Ok (scope, nl)

_builtin_int_to_string scope args = 
  do 
    l <- if length args == 1 then Ok (head args) else Bad "Too many argument for int_to_string(int)."
    checkLocType l (Builtin "int") scope
    (scope, nl) <- popLocation scope
    i <- fetchInt l scope
    scope <- addString nl (show i) scope
    Ok (scope, nl)


_builtin_print scope args = 
  do 
    l <- if length args == 1 then Ok (head args) else Bad "Too many arguments for print."
    checkLocType l (Builtin "string") scope
    str <- fetchString l scope
    Ok (scope {outs = (str:(outs scope))})

_builtin_crash scope args = 
  do 
    l <- if length args == 1 then Ok (head args) else Bad "Too many arguments for crash."
    checkLocType l (Builtin "string") scope
    str <- fetchString l scope
    Bad ("Program crashed with message: " ++ str)