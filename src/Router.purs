module Router where

import BigPrelude
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)
import Component.Profile as Profile
import Component.Items as Items
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Functor.Coproduct (Coproduct)
import Data.String (toLower)
import Halogen.Component.ChildPath (ChildPath, cpR, cpL)

data Input a
  = Goto Routes a

data CRUD
  = Index
  | Show Number

data Routes
  = Profile
  | Items CRUD
  | Home

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = profile
      <|> items
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    items = Items <$> (route "items" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

type State =
  { currentPage :: String
  }

type ChildQuery = Coproduct Profile.Input Items.Input
type ChildSlot = Either Profile.Slot Items.Slot

pathToProfile :: ChildPath Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToItems :: ChildPath Items.Input ChildQuery Items.Slot ChildSlot
pathToItems = cpR

type QueryP
  = Coproduct Input ChildQuery

ui :: forall m. H.Component HH.HTML Input Unit Void m
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Input ChildQuery ChildSlot m
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (st.currentPage) ]
        , HH.ul_ (map link ["Items", "Profile", "Home"])
        , viewPage st.currentPage
        ]

    link s = HH.li_ [ HH.a [ HP.href ("#/" <> toLower s) ] [ HH.text s ] ]

    viewPage :: String -> H.ParentHTML Input ChildQuery ChildSlot m
    viewPage "Items" =
      HH.slot' pathToItems Items.Slot Items.ui unit absurd      
    viewPage "Profile" =
      HH.slot' pathToProfile Profile.Slot Profile.ui unit absurd
    viewPage _ =
      HH.div_ []

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void m
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (Goto (Items view) next) = do
      modify case view of
                  Index -> (_ { currentPage = "Items" })
                  Show n -> (_ { currentPage = "Item " <> show n })
      pure next      
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next

routeSignal :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
-- redirects driver _ Home =
--   driver (left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (left (action (Goto Profile))))
-- redirects driver _ (Items view) =
--   driver (left (action (Goto (Items view)))))