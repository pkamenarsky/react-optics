{-# LANGUAGE DeriveFunctor #-}

module Main where

import Lib

data Attrs st = OnClick (st -> st)

_1 :: (((a, b) -> a), (a -> (a, b) -> (a, b)))
_1 = (fst, \a (_, b) -> (a, b))

_2 :: (((a, b) -> b), (b -> (a, b) -> (a, b)))
_2 = (snd, \b (a, _) -> (a, b))

mapAttrs :: (st' -> st, st -> st' -> st') -> Attrs st -> Attrs st'
mapAttrs (get, set) (OnClick f) = OnClick (\st -> set (f (get st)) st)

data Html st = Div [Attrs st] [Html st] | Text String

mapHtml :: (st' -> st, st -> st' -> st') -> Html st -> Html st'
mapHtml lns (Div attrs children) = Div (map (mapAttrs lns) attrs) (map (mapHtml lns) children)

--------------------------------------------------------------------------------

type Component st = st -> Html st

zoom :: (st' -> st, st -> st' -> st') -> st' -> Component st -> Html st'
zoom lns@(get, _) st cmp = mapHtml lns (cmp (get st))

button :: Component Bool
button toggled = Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle True  = False
        toggle False = True

ui :: Component (Bool, Bool)
ui st = Div [] [ zoom _1 st button, zoom _2 st button ]

--------------------------------------------------------------------------------

main :: IO ()
main = someFunc
