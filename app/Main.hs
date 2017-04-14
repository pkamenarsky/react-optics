{-# LANGUAGE DeriveFunctor #-}

module Main where

import Lib

data Effect st =
    Pure (st -> st)
  | GetHTTP String (String -> Effect st)

mapEffect :: (st' -> st, st -> st' -> st') -> Effect st -> Effect st'
mapEffect (get, set) (Pure f) = Pure (\st -> set (f (get st)) st)
mapEffect lns (GetHTTP url eff) = (GetHTTP url $ \r -> (mapEffect lns (eff r)))

data Attrs st =
    OnAttach (Effect st)
  | OnClick (Effect st)

_1 :: (((a, b) -> a), (a -> (a, b) -> (a, b)))
_1 = (fst, \a (_, b) -> (a, b))

_2 :: (((a, b) -> b), (b -> (a, b) -> (a, b)))
_2 = (snd, \b (a, _) -> (a, b))

mapAttrs :: (st' -> st, st -> st' -> st') -> Attrs st -> Attrs st'
mapAttrs lns (OnAttach eff) = OnAttach (mapEffect lns eff)
mapAttrs lns (OnClick eff) = OnClick (mapEffect lns eff)

data Html st = Div [Attrs st] [Html st] | Text String

mapHtml :: (st' -> st, st -> st' -> st') -> Html st -> Html st'
mapHtml lns (Div attrs children) = Div (map (mapAttrs lns) attrs) (map (mapHtml lns) children)

--------------------------------------------------------------------------------

type Component st = st -> Html st

zoom :: (st' -> st, st -> st' -> st') -> st' -> Component st -> Html st'
zoom lns@(get, _) st cmp = mapHtml lns (cmp (get st))

ajax :: Component String
ajax str = Div [ OnAttach init, OnClick fetch ] [ Text str ]
  where init  = GetHTTP ("google.com/q=init") $ \res -> Pure (const res)
        fetch = GetHTTP ("google.com/q=" ++ str) $ \res -> Pure (const res)

button :: Component Bool
button toggled = Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle = Pure $ \st -> case st of
          True  -> False
          False -> True

ui :: Component (Bool, Bool)
ui st = Div [] [ zoom _1 st button, zoom _2 st button ]

--------------------------------------------------------------------------------

main :: IO ()
main = someFunc
