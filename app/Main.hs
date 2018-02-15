{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty as V

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Clock

import Control.Monad (void)
import qualified Data.List as L

import Text.Printf (printf)

data Month = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Enum)

daysOfMonth :: Integer -> Int -> [Cal.Day]
daysOfMonth year month = enumFromTo firstDay lastDay
 where
  firstDay    = Cal.fromGregorian year month 1
  lastDay     = Cal.addDays monthLength firstDay
  monthLength = fromIntegral (Cal.gregorianMonthLength year month) - 1

drawUi :: Cal.Day -> [Widget n]
drawUi today = [ui] where ui = renderYear 2018 today

renderYear :: Integer -> Cal.Day -> Widget n
renderYear year today =
  withBorderStyle unicode
    $ borderWithLabel (str ("Year " ++ show year))
    $ hBox
    $ L.intersperse vBorder (map (renderMonth today year) [1 .. 12])

renderMonth :: Cal.Day -> Integer -> Int -> Widget n
renderMonth today year month = vBox (title : hBorder : days)
 where
  title      = (style . hCenter . str) monthName
  days       = map (renderDay today) (daysOfMonth year month)
  monthName  = show (toEnum (month - 1) :: Month)
  (y, m, _d) = Cal.toGregorian today
  style      = if y == year && m == month then withAttr bold else id

renderDay :: Cal.Day -> Cal.Day -> Widget n
renderDay today day = (style . str . concat) [date, " ", dayOfWeekString]
 where
  (_y, _m, d)     = Cal.toGregorian day
  date            = printf "%2d" d
  dayOfWeek       = Cal.dayOfWeek day
  dayOfWeekString = [head (show dayOfWeek)]
  style           = if
    | dayOfWeek `elem` [Cal.Saturday, Cal.Sunday] -> withAttr inversed
    | day == today -> withAttr bold
    | otherwise    -> id

colored :: AttrName
colored = attrName "colored"

inversed :: AttrName
inversed = attrName "inversed"

bold :: AttrName
bold = attrName "bold"

appEvent :: Cal.Day -> BrickEvent n e -> EventM n (Next Cal.Day)
appEvent today = \case
  (VtyEvent (V.EvKey V.KEsc [])) -> halt today
  _                              -> continue today

app :: App Cal.Day e ()
app = App
  { appDraw         = drawUi
  , appStartEvent   = return
  , appHandleEvent  = appEvent
  , appAttrMap      = const $ attrMap
    V.defAttr
    [ (colored , bg V.blue)
    , (inversed, V.withStyle V.defAttr V.reverseVideo)
    , (bold    , V.withStyle V.defAttr V.bold)
    ]
  , appChooseCursor = neverShowCursor
  }

main :: IO ()
main = do
  today <- Clock.utctDay <$> Clock.getCurrentTime
  void $ defaultMain app today