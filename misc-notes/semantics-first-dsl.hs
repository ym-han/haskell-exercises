-- code from "Semantics First: rethinking the langauge design process"

-- data Map a b = a :-> b | Map a b :&: Map a b -- old version
data Map w a b = w a :-> w b | Map w a b :&: Map w a b | Wrap (w (Map w a b))

------ time dsl
type Hour = Int
type Minute = Int
data Time = T Hour Minute

hours h = T h 0
am h = hours h
pm h = hours (h + 12)
before t t' = t' - t

------ date dsl
data Month = Jan | Feb | March | Apr | Dec
type Day = Int
data Date = D Month Day

[jan, feb, march, apr, dec] = map D [ Jan, Feb, March, Apr, Dec]

------ mapping them
type CalT a = Map Time a
type CalD a = Map Date a 
type Cal a = CalD (CalT a)


type CalL a = Cal (a,Time)
{-
            = CalD (CalT (a, Time))
            = Map Date (CalT (a, Time))
            = Map Date (Map Time (a, Time))

This defines a calendar domain in which each appointment is a pair of some arbitrary appointment value and a time value representing the length of the appointment.

This extension is modular in the sense that
the change to the semantic domain is localized, allowing us to directly reuse any syntax or operations that are polymorphic in the appointment subdomain (that is, that have types like Cal a).
-}

data Privacy k a = Hidden k a | Public a deriving (Show)

type Key = String
type Private a = Privacy Key a
-- type CalP a = Map (Private Date) (Private a)
-- type CalPPte a = CalP (Private a) 

(*->.) :: (Key, Date) -> a -> Map Private a b
(k, d) *->. i = Hidden k d :-> Public i

(.->*) :: Date -> (Key, a) -> Map Private a b
d .->* (k, i) = Public d :-> Hidden k i

(.->.) :: Date -> a -> Map Private a b
d .->. i = Public d :-> Public i

-- Wrap (w (Map w a b))
week1 :: Map Private Date String
week1 = Wrap $ Hidden "pwd" ((jan 1 .->. "Party") :&: (jan 2 .->. "Rest"))






