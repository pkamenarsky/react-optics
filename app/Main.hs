{-# LANGUAGE DeriveFunctor #-}

module Main where

import Lib

data Effect pst st =
    Pure (st -> st)
  | GetHTTP String (String -> Effect pst st)
  | Parent (pst -> pst)

mapEffect :: (st' -> st, st -> st' -> st') -> Effect pst st -> Effect pst st'
mapEffect (get, set) (Pure f) = Pure (\st -> set (f (get st)) st)
mapEffect lns (GetHTTP url eff) = GetHTTP url $ \r -> (mapEffect lns (eff r))
mapEffect lns (Parent f) = Parent f

type Lens st' st = (st' -> st, st -> st' -> st')

data Attrs pst st =
    OnAttach (Effect pst st)
  | OnClick (Effect pst st)

_1 :: (((a, b) -> a), (a -> (a, b) -> (a, b)))
_1 = (fst, \a (_, b) -> (a, b))

_2 :: (((a, b) -> b), (b -> (a, b) -> (a, b)))
_2 = (snd, \b (a, _) -> (a, b))

mapAttrs :: Lens st' st -> Attrs pst st -> Attrs pst st'
mapAttrs lns (OnAttach eff) = OnAttach (mapEffect lns eff)
mapAttrs lns (OnClick eff) = OnClick (mapEffect lns eff)

data Html pst st = Div [Attrs pst st] [Html pst st] | Text String

mapHtml :: Lens st' st -> Html pst st -> Html pst st'
mapHtml lns (Div attrs children) = Div (map (mapAttrs lns) attrs) (map (mapHtml lns) children)

--------------------------------------------------------------------------------

type Component pst st = st -> Html pst st

zoom :: Lens st' st -> st' -> Component st' st -> Html st'' st'
zoom lns@(get, _) st cmp = mapHtml lns (undefined (get st))

ajax :: Component (Bool, String) String
ajax str = Div [ OnAttach init, OnClick fetch, OnClick parent ] [ Text str ]
  where init  = GetHTTP ("google.com/q=init") $ \res -> Pure (const res)
        fetch = GetHTTP ("google.com/q=" ++ str) $ \res -> Pure (const res)
        parent = Parent $ \(b, str) -> (not b, str)

button :: Component st Bool
button toggled = Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle = Pure $ \st -> case st of
          True  -> False
          False -> True

ui :: Component (Bool, String) (Bool, String)
ui st = Div [] [ zoom _1 st button, zoom _2 st ajax ]

--------------------------------------------------------------------------------

main :: IO ()
main = someFunc
