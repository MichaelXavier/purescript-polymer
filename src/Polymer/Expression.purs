module Polymer.Expression where

-------------------------------------------------------------------------------
--TODO: move to types
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
class E (i :: * -> * -> *) where
  lit :: forall p a. (Show a) => a -> i p a

  -- comparison
  eq :: forall p b. (Eq b) => i p b -> i p b -> i p Boolean
  neq :: forall p b. (Eq b) => i p b -> i p b -> i p Boolean
  lt :: forall p b. (Ord b) => i p b -> i p b -> i p Boolean
  lte :: forall p b. (Ord b) => i p b -> i p b -> i p Boolean
  gt :: forall p b. (Ord b) => i p b -> i p b -> i p Boolean
  gte :: forall p b. (Ord b) => i p b -> i p b -> i p Boolean

  -- math, TODO: group
  plus :: forall p. i p Number -> i p Number -> i p Number
  minus :: forall p. i p Number -> i p Number -> i p Number
  mult :: forall p. i p Number -> i p Number -> i p Number
  div' :: forall p. i p Number -> i p Number -> i p Number
  mod:: forall p. i p Number -> i p Number -> i p Number

  -- Boolean logic
  not' :: forall p. i p Boolean -> i p Boolean
  and :: forall p. i p Boolean -> i p Boolean -> i p Boolean
  or :: forall p. i p Boolean -> i p Boolean -> i p Boolean
  tern :: forall p a. i p Boolean -> i p a -> i p a -> i p a

  --TODO: need some way to tie the prop to the prototype
  ref :: forall p b. Prop p b -> i p b
  --TODO: function application, deep paths

-------------------------------------------------------------------------------
newtype PP p a = PP String

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
renderExpression :: forall p a. Expression p a -> String
renderExpression e = "{{" ++ runPP e ++ "}}"

type Expression = PP

