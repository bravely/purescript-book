module Test.MySolutions where

import Prelude
import Math (sqrt, pi)
import Data.Int (rem)

diagonal w h = sqrt (w * w + h * h)

circleArea radius = pi * radius * radius

leftoverCents dollars = rem dollars 100
