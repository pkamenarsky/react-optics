{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StaticPointers #-}

module Main where

import GHC.StaticPtr

import Lib

data Effect st' st =
    Pure (st -> st)
  | RemoteF (StaticPtr (st -> st))
  | Parent (st' -> st')
  | GetHTTP String (String -> Effect st' st)

mapEffect :: (st' -> st, st -> st' -> st') -> Effect st' st -> Effect st'' st'
mapEffect (get, set) (Pure f) = Pure (\st -> set (f (get st)) st)
mapEffect lns (GetHTTP url eff) = GetHTTP url $ \r -> (mapEffect lns (eff r))
mapEffect lns (Parent f) = Pure f

type Lens st' st = (st' -> st, st -> st' -> st')

data Attrs pst st =
    OnAttach (Effect pst st)
  | OnClick (Effect pst st)

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

data Remote st = Remote st

remotely :: StaticPtr (a -> b) -> Remote a -> b
remotely = undefined

modifyRemotely :: StaticPtr (a -> b) -> Remote a -> Remote b
modifyRemotely = undefined

combine :: Remote a -> Remote b -> Remote (a, b)
combine = undefined

remoteF :: StaticPtr (a -> b) -> a -> b
remoteF = undefined

--------------------------------------------------------------------------------

type ChildComponent st' st = st -> Html st' st

type Component st = forall st'. ChildComponent st' st

zoom :: Lens st' st -> st' -> ChildComponent st' st -> Html st'' st'
zoom lns@(get, _) st cmp = mapHtml lns (cmp (get st))

zoomRemote :: Lens st' (Remote st) -> st' -> ChildComponent st' st -> Html st'' st'
-- zoomRemote lns@(get, _) st cmp = mapHtml lns (cmp (get st))
zoomRemote lns@(get, _) st cmp = undefined

--------------------------------------------------------------------------------

ajax :: (st -> st) -> ChildComponent st String
ajax parst str = Div [ OnAttach init, OnClick fetch, OnClick parent ] [ Text str ]
  where init  = GetHTTP ("google.com/q=init") $ \res -> Pure (const res)
        fetch = GetHTTP ("google.com/q=" ++ str) $ \res -> Pure (const res)
        parent = Parent parst

button :: Component Bool
button toggled = Div [ OnClick toggle ] [ Text $ if toggled then "On" else "Off" ]
  where toggle = Pure $ \st -> case st of
          True  -> False
          False -> True

ui :: Component (Remote Bool, String)
ui st = Div [] [ zoomRemote _1 st button, zoom _2 st (ajax $ \(a, b) -> (modifyRemotely (static not) a, b ++ "str")) ]
  where notR = remotely (static not) (fst st)
        str = remoteF (static length) (snd st)

--------------------------------------------------------------------------------

runComponent :: st -> ChildComponent () st -> IO ()
runComponent = undefined

main :: IO ()
main = runComponent (Remote True, "string") ui
