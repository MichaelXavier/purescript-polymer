module Polymer.Expression where

-------------------------------------------------------------------------------
data ProtoAttr proto a = ProtoAttr String (proto -> a)


-------------------------------------------------------------------------------
class E (i :: * -> *) where
  lit :: forall a. (Show a) => a -> i a

  -- comparison
  eq :: forall b. (Eq b) => i b -> i b -> i Boolean
  neq :: forall b. (Eq b) => i b -> i b -> i Boolean
  lt :: forall b. (Ord b) => i b -> i b -> i Boolean
  lte :: forall b. (Ord b) => i b -> i b -> i Boolean
  gt :: forall b. (Ord b) => i b -> i b -> i Boolean
  gte :: forall b. (Ord b) => i b -> i b -> i Boolean

  -- math, TODO: group
  plus :: i Number -> i Number -> i Number
  minus :: i Number -> i Number -> i Number
  mult :: i Number -> i Number -> i Number
  div' :: i Number -> i Number -> i Number
  mod:: i Number -> i Number -> i Number

  -- Boolean logic
  not' :: i Boolean -> i Boolean
  and ::  i Boolean -> i Boolean -> i Boolean
  or ::  i Boolean -> i Boolean -> i Boolean
  tern ::  forall a. i Boolean -> i a -> i a -> i a

  ref :: forall proto a. ProtoAttr proto a -> i a
  --TODO: function application, deep paths

-------------------------------------------------------------------------------
newtype PP a = PP String

binop op a b = grouped $ runPP a ++ " " ++ op ++ " " ++ runPP b

grouped s = PP $ "(" ++ s ++ ")"

-------------------------------------------------------------------------------
--TOOD: dry up some of this
instance ePretty :: E PP where
  lit x = PP $ show x

  eq = binop "==="
  neq  = binop "!=="
  lt = binop "<"
  lte = binop "<="
  gt = binop ">"
  gte = binop ">="

  plus = binop "+"
  minus = binop "-"
  mult = binop "*"
  div' = binop "/"
  mod = binop "*"


  not' a = PP $ "!" ++ runPP a
  and = binop "&&"
  or = binop "||"
  tern p a b = PP $ runPP p ++ " ? " ++ runPP a ++ " : " ++ runPP b

  ref (ProtoAttr s _) = PP s


-------------------------------------------------------------------------------
runPP (PP a) = a


-------------------------------------------------------------------------------
--TODO: only export this
renderExpression :: forall a. Expression a -> String
renderExpression e = "{{" ++ runPP e ++ "}}"


-------------------------------------------------------------------------------
--TODO: drop this example code
data MyProto = MyProto Boolean

protoVal (MyProto n) = n

protoValAttr = ProtoAttr "protoVal" protoVal

example :: String
example = runPP $ ((lit 5 `minus` lit 2) `mod` (lit 2)) `gte` lit 3


-------------------------------------------------------------------------------
type Expression a = PP a

