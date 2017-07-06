{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE StaticPointers #-}

module Main where

import Data.Monoid

import Data.IORef
import System.IO.Unsafe

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

-- remotely :: StaticPtr (a -> b) -> a -> b
-- remotely = undefined

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

{-
ui :: Component (Bool, String)
ui st = Div [] [ zoom _1 st button, zoom _2 st (ajax $ static (\(a, b) -> (not a, b ++ "str"))) ]
  where localbool = remotely (static id) (fst st)
-}

--------------------------------------------------------------------------------

main :: IO ()
main = someFunc

--------------------------------------------------------------------------------

data RemoteMap k v = RemoteMap (IORef [(k, v)]) (IO ())

request :: req -> (rsp -> IO ()) -> IO ()
request = undefined

get :: Ord k => k -> RemoteMap k v -> Maybe v
get k (RemoteMap cache render) = unsafePerformIO $ do
  cache' <- readIORef cache

  case lookup k cache' of
    Just v  -> return $ Just v
    Nothing -> do
      request k $ \v -> do
        modifyIORef cache ((k, v):)
        render
      -- throw exception
      return undefined

--------------------------------------------------------------------------------

type BBox = (Int, Int, Int, Int)

data Layout = Layout { layoutBBoxes :: [BBox] -> [BBox] -> [BBox], layoutChildren :: [Layout] }
data UI = UI { bboxes :: [BBox], uiChildren :: [UI] } deriving Show

mkUI :: [BBox] -> Layout -> UI
mkUI pBBoxes layout = UI myBBoxes chUI
  where
    chUI = map (mkUI myBBoxes) (layoutChildren layout)
    myBBoxes = layoutBBoxes layout pBBoxes (concatMap bboxes chUI)

fixedLayout :: Layout
fixedLayout = Layout layout []
  where
    layout _ _ = [(0, 0, 10, 10)]

responsiveLayout :: Layout
responsiveLayout = Layout layout [fixedLayout]
  where
    layout _ ch = ch

--------------------------------------------------------------------------------

data Static a = Static a

class Remotely a where
  remotely :: Static (a -> r) -> a -> r

instance {-# OVERLAPPABLE #-} Remotely a where
  remotely (Static ptr) a = ptr a

instance Remotely r => Remotely (a -> r) where
  remotely (Static ptr) a = remotely (Static $ \a -> ptr a) a

plus :: Static (Int -> Int -> Int)
plus = Static (+)

-- testR :: _
testR = remotely plus 4 5
