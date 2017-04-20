{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StaticPointers #-}

module Main where

import GHC.StaticPtr

import Lib

data Effect st' st =
    Pure (st -> st)
  | Remote (StaticPtr (st -> st))
  | Parent (st' -> st')
  | RemoteParent (StaticPtr (st' -> st'))
  | GetHTTP String (String -> Effect st' st)

mapEffect :: (st' -> st, st -> st' -> st') -> Effect st' st -> Effect st'' st'
mapEffect (get, set) (Pure f) = Pure (\st -> set (f (get st)) st)
mapEffect lns (GetHTTP url eff) = GetHTTP url $ \r -> (mapEffect lns (eff r))
mapEffect lns (Parent f) = Pure f

type Lens st' st = (st' -> st, st -> st' -> st')

data Attrs pst st =
    OnAttach (Effect pst st)
  | OnClick (Effect pst st)
  | OnFetch (Effect pst st) (Effect pst st) -- used to set/unset loading indicator

_1 :: (((a, b) -> a), (a -> (a, b) -> (a, b)))
_1 = (fst, \a (_, b) -> (a, b))

_2 :: (((a, b) -> b), (b -> (a, b) -> (a, b)))
_2 = (snd, \b (a, _) -> (a, b))

mapAttrs :: Lens st' st -> Attrs st' st -> Attrs st'' st'
mapAttrs lns (OnAttach eff) = OnAttach (mapEffect lns eff)
mapAttrs lns (OnClick eff) = OnClick (mapEffect lns eff)

data Html pst st = Div [Attrs pst st] [Html pst st] | Text String

mapHtml :: Lens st' st -> Html st' st -> Html pst' st'
mapHtml lns (Div attrs children) = Div (map (mapAttrs lns) attrs) (map (mapHtml lns) children)

--------------------------------------------------------------------------------

data ChildComponent' st' st = ChildComponent'
  { render :: st -> Html st' st
  , attach :: Effect st' st
  , detach :: Effect st' st
  , fetch  :: (Effect st' st, Effect st' st)
  }

type Component' st = forall st'. ChildComponent' st' st

simpleComponent :: (st -> Html st' st) -> ChildComponent' st' st
simpleComponent render = ChildComponent'
  { render = render
  , attach = Pure id
  , detach = Pure id
  , fetch  = (Pure id, Pure id)
  }

type ChildComponent st' st = st -> Html st' st

type Component st = forall st'. ChildComponent st' st

remotely :: StaticPtr (a -> b) -> a -> b
remotely = undefined

zoom :: Lens st' st -> st' -> ChildComponent st' st -> Html st'' st'
zoom lns@(get, _) st cmp = mapHtml lns (cmp (get st))

zoomEff :: (st' -> st) -> (st -> Effect st'' st') -> st' -> ChildComponent st' st -> Html st'' st'
zoomEff = undefined

ajax :: StaticPtr (st -> st) -> ChildComponent st String
ajax parst str = Div [ OnAttach init, OnClick fetch, OnClick parent ] [ Text str ]
  where init  = GetHTTP ("google.com/q=init") $ \res -> Pure (const res)
        fetch = GetHTTP ("google.com/q=" ++ str) $ \res -> Pure (const res)
        parent = RemoteParent parst

button :: Component Bool
button toggled = Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle = Pure $ \st -> case st of
          True  -> False
          False -> True

button' :: Component' Bool
button' = simpleComponent $ \toggled -> Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle = Pure $ \st -> case st of
          True  -> False
          False -> True

ui :: Component (Bool, String)
ui st = Div [] [ zoom _1 st button, zoom _2 st (ajax $ static (\(a, b) -> (not a, b ++ "str"))) ]
  where localbool = remotely (static id) (fst st)

--------------------------------------------------------------------------------

main :: IO ()
main = someFunc
