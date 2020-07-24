module Dunbar.StateTest where

import Prelude

import Data.Array (foldr, reverse)
import Data.Bifunctor (lmap, rmap)
import Data.Tuple (Tuple(..))
import Data.DateTime.Instant (Instant)
import Data.Newtype (wrap)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Dunbar.Friend as Fr
import Dunbar.State as St
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, fail)
import Utils.DateTime (dateToInstant, daysToInt, newDate)
import Utils.Lens as L

stateFromEvents ::  Array St.Event -> St.State
stateFromEvents = reverse >>> foldr St.processEvent St.empty

overdueContactsSimplified :: Instant -> St.State -> Array (Tuple String Int)
overdueContactsSimplified i = St.overdueContacts i
                              >>> (map (lmap (L.view Fr._name >>> show)))
                              >>> (map (rmap daysToInt))

dateInstant :: Int -> Int -> Int -> Instant
dateInstant y m d = dateToInstant $ newDate y m d

overdueContactsSpec :: Spec Unit
overdueContactsSpec = describe "overdueContacts" do
  it "can get list of overdue contacts, from most overdue to least overdue" do
    let state = stateFromEvents [ St.addFriendEvent "Jordan" "Henderson"
                                , St.addFriendEvent "Naby" "Keita"
                                , St.addFriendEvent "Curtis" "Jones"
                                , St.updateDesiredContactFrequencyEvent (wrap 0) (Just 7)
                                , St.justSeenEvent (wrap 0) (dateInstant 2020 1 1)
                                , St.updateDesiredContactFrequencyEvent (wrap 2) (Just 21)
                                , St.justSeenEvent (wrap 2) (dateInstant 2020 1 10)
                                ]
    let now = dateToInstant $ newDate 2020 1 10
    overdueContactsSimplified now (spy "state" state) `shouldEqual` [
      (Tuple "Jordan Henderson" 2)
  ]

main :: Spec Unit
main = do
  overdueContactsSpec
