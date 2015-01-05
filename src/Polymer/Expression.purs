module Polymer.Expression where

-------------------------------------------------------------------------------
newtype Prop a b = Prop (a -> b)

instance semigroupoidProp :: Semigroupoid Prop where
  (<<<) (Prop f) (Prop g) = Prop (f <<< g)

instance categoryProp :: Category Prop where
  id = Prop id


--WARNING: meant to be used in Compiler with either ES6 proxy support
--or (more likely) node --harmony-proxies

foreign import propPath """
  function propPath(f) {
    var path = [],
        proxy = Proxy.create({
          get: function(_, prop) {
            path.push(prop);
            return proxy;
          }
        });
    f(proxy);
    return path.join('.');
  }
""" :: forall a b. (Prop a b) -> String

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

  --TODO: need some way to tie the prop to the prototype
  ref :: forall a b. Prop a b -> i b
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

  ref p = PP $ propPath p


-------------------------------------------------------------------------------
runPP (PP a) = a


-------------------------------------------------------------------------------
--TODO: only export this
renderExpression :: forall a. Expression a -> String
renderExpression e = "{{" ++ runPP e ++ "}}"

type Expression = PP

