module Nanotime
  ( TimeDelta (..)
  , timeDeltaFromFracSecs
  , timeDeltaFromNanos
  , timeDeltaToFracSecs
  , timeDeltaToNanos
  , diffTimeDelta
  , threadDelayDelta
  , TimeLike (..)
  , awaitDelta
  , PosixTime (..)
  , MonoTime (..)
  , monoTimeToFracSecs
  , monoTimeToNanos
  , monoTimeFromFracSecs
  , monoTimeFromNanos
  , NtpTime (..)
  , posixToNtp
  , ntpToPosix
  , assertingNonNegative
  )
where

import Control.Concurrent (threadDelay)
import Data.Bits (Bits (..))
import Data.Fixed (Fixed (..), Pico)
import Data.Semigroup (Sum (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

assertingNonNegative :: (HasCallStack, Ord a, Num a, Show a) => a -> a
assertingNonNegative a =
  if a < 0
    then error ("Required non-negative value but got " ++ show a)
    else a

-- | Non-negative time difference in nanoseconds since last event
-- Like a 'Nano' (`Fixed E9`) but a machine word.
newtype TimeDelta = TimeDelta {unTimeDelta :: Word64}
  deriving stock (Eq, Show, Ord, Generic, Bounded)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via (Sum Word64)

-- | Return a 'TimeDelta' corresponding the the given number of fractional seconds.
-- (For example, 1.5 represents one and a half seconds.)
timeDeltaFromFracSecs :: (Real a, Show a) => a -> TimeDelta
timeDeltaFromFracSecs d = TimeDelta (round (1000000000 * toRational (assertingNonNegative d)))

-- | Return a 'TimeDelta' corresponding the the given number of nanoseconds.
-- (For example, 1000000000 represends one second.)
timeDeltaFromNanos :: (Integral a, Show a) => a -> TimeDelta
timeDeltaFromNanos = TimeDelta . fromIntegral . assertingNonNegative

timeDeltaToFracSecs :: Fractional a => TimeDelta -> a
timeDeltaToFracSecs (TimeDelta n) = fromIntegral n / 1000000000

timeDeltaToNanos :: TimeDelta -> Word64
timeDeltaToNanos = unTimeDelta

-- | Return the difference of two time deltas
diffTimeDelta
  :: TimeDelta
  -- ^ the "larger" delta
  -> TimeDelta
  -- ^ the "smaller" delta
  -> Maybe TimeDelta
  -- ^ difference between the two (Nothing if negative)
diffTimeDelta (TimeDelta big) (TimeDelta small) =
  if big <= small
    then Nothing
    else Just (TimeDelta (big - small))

threadDelayDelta :: TimeDelta -> IO ()
threadDelayDelta (TimeDelta td) = threadDelay (fromIntegral (div td 1000))

class Ord t => TimeLike t where
  diffTime :: t -> t -> Maybe TimeDelta
  addTime :: t -> TimeDelta -> t
  currentTime :: IO t

awaitDelta :: TimeLike t => t -> TimeDelta -> IO t
awaitDelta m t = do
  let target = addTime m t
  cur <- currentTime
  case diffTime target cur of
    Nothing -> pure cur
    Just td -> target <$ threadDelayDelta td

newtype PosixTime = PosixTime {unPosixTime :: Word64}
  deriving stock (Eq, Show, Ord, Generic, Bounded)

e9W :: Word64
e9W = 1000000000

picoToNanoWord :: Pico -> Word64
picoToNanoWord (MkFixed i) = fromInteger (div i 1000)

picoFromNanoWord :: Word64 -> Pico
picoFromNanoWord j = MkFixed (1000 * toInteger j)

instance TimeLike PosixTime where
  diffTime (PosixTime t2) (PosixTime t1) =
    if t2 <= t1 then Nothing else Just (TimeDelta (t2 - t1))
  addTime (PosixTime t) (TimeDelta d) = PosixTime (t + d)
  currentTime = fmap (PosixTime . picoToNanoWord . nominalDiffTimeToSeconds) getPOSIXTime

-- | Monotonic time in nanoseconds since some unspecified epoch (see 'getMonotonicTimeNs')
newtype MonoTime = MonoTime {unMonoTime :: Word64}
  deriving stock (Eq, Show, Ord, Generic, Bounded)

monoTimeFromFracSecs :: (Real a, Show a) => a -> MonoTime
monoTimeFromFracSecs d = MonoTime (round (1000000000 * toRational (assertingNonNegative d)))

monoTimeFromNanos :: (Integral a, Show a) => a -> MonoTime
monoTimeFromNanos = MonoTime . fromIntegral . assertingNonNegative

monoTimeToFracSecs :: Fractional a => MonoTime -> a
monoTimeToFracSecs (MonoTime n) = fromIntegral n / 1000000000

monoTimeToNanos :: MonoTime -> Word64
monoTimeToNanos = unMonoTime

instance TimeLike MonoTime where
  diffTime (MonoTime t2) (MonoTime t1) =
    if t2 <= t1 then Nothing else Just (TimeDelta (t2 - t1))
  addTime (MonoTime t) (TimeDelta d) = MonoTime (t + d)
  currentTime = fmap MonoTime getMonotonicTimeNSec

newtype NtpTime = NtpTime {unNtpTime :: Word64}
  deriving stock (Eq, Show, Ord, Generic, Bounded)

nanoWordToSplit :: Word64 -> (Word32, Word32)
nanoWordToSplit j =
  let whole = div j e9W
      part = j - e9W * whole
  in  (fromIntegral whole, fromIntegral part)

nanoWordFromSplit :: Word32 -> Word32 -> Word64
nanoWordFromSplit whole part = e9W * fromIntegral whole + fromIntegral part

ntpFromSplit :: Word32 -> Word32 -> NtpTime
ntpFromSplit whole part = NtpTime (shiftL (fromIntegral whole) 32 .|. fromIntegral part)

ntpToSplit :: NtpTime -> (Word32, Word32)
ntpToSplit (NtpTime k) = (fromIntegral (shiftR k 32), fromIntegral k)

-- Difference in nano seconds between 1/1/1900 and 1/1/1970
-- 1900 is the NTP epoch, 1970 is the unix epoch
ntpEpochDiffSeconds :: Word32
ntpEpochDiffSeconds = 2208988800

posixToNtp :: PosixTime -> NtpTime
posixToNtp (PosixTime j) =
  let (whole, part) = nanoWordToSplit j
      whole' = whole + ntpEpochDiffSeconds
  in  ntpFromSplit whole' part

ntpToPosix :: NtpTime -> PosixTime
ntpToPosix k =
  let (whole, part) = ntpToSplit k
      whole' = whole - ntpEpochDiffSeconds
  in  PosixTime (nanoWordFromSplit whole' part)

-- Probably best to do time arithmetic directly on PosixTime
instance TimeLike NtpTime where
  diffTime n2 n1 = diffTime (ntpToPosix n2) (ntpToPosix n1)
  addTime n d = posixToNtp (addTime (ntpToPosix n) d)
  currentTime = fmap posixToNtp currentTime

