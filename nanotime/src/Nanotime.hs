module Nanotime
  ( Sign (..)
  , TimeDelta (..)
  , timeDeltaFromFracSecs
  , timeDeltaFromNanos
  , timeDeltaToFracSecs
  , timeDeltaToNanos
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
  )
where

import Control.Concurrent (threadDelay)
import Data.Bits (Bits (..))
import Data.Fixed (Fixed (..), Pico)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stack (HasCallStack)

-- | Sign (negative or positive) of a magnitude of time difference
data Sign = SignNeg | SignPos
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Signed time difference in nanoseconds since last event
-- Like a 'Nano' (`Fixed E9`) but using a machine word with explicit sign.
data TimeDelta = TimeDelta
  { tdSign :: !Sign
  , tdMag :: !Word64
  }
  deriving stock (Show)

instance Eq TimeDelta where
  TimeDelta s1 m1 == TimeDelta s2 m2 =
    (m1 == 0 && m2 == 0) || (s1 == s2 && m1 == m2)

instance Ord TimeDelta where
  compare (TimeDelta s1 m1) (TimeDelta s2 m2) =
    if m1 == 0 && m2 == 0
      then EQ
      else case s1 of
        SignPos ->
          case s2 of
            SignPos -> compare m1 m2
            SignNeg -> GT
        SignNeg ->
          case s2 of
            SignPos -> LT
            SignNeg -> compare m2 m1

instance Bounded TimeDelta where
  minBound = TimeDelta SignNeg maxBound
  maxBound = TimeDelta SignPos maxBound

instance Semigroup TimeDelta where
  td1@(TimeDelta s1 m1) <> td2@(TimeDelta s2 m2) =
    if
      | m1 == 0 -> td2
      | m2 == 0 -> td1
      | s1 == s2 -> mkTimeDelta s1 (m1 + m2)
      | otherwise ->
          if m1 >= m2
            then mkTimeDelta s1 (m1 - m2)
            else mkTimeDelta s2 (m2 - m1)

instance Monoid TimeDelta where
  mempty = TimeDelta SignPos 0

instance Num TimeDelta where
  (+) = (<>)
  (*) = error "TimeDelta multiplication has no meaning"
  abs (TimeDelta _ m) = TimeDelta SignPos m
  signum (TimeDelta s m) =
    if m == 0
      then 0
      else case s of
        SignPos -> 1
        SignNeg -> -1
  fromInteger i =
    if i >= 0
      then TimeDelta SignPos (fromInteger i)
      else TimeDelta SignNeg (fromInteger (negate i))
  negate td@(TimeDelta s m) =
    if m == 0 && s == SignPos
      then td
      else case s of
        SignPos -> TimeDelta SignNeg m
        SignNeg -> TimeDelta SignPos m

-- private
mkTimeDelta :: Sign -> Word64 -> TimeDelta
mkTimeDelta s m =
  if m == 0
    then TimeDelta SignPos m
    else TimeDelta s m

-- | Return a 'TimeDelta' corresponding the the given number of fractional seconds.
-- (For example, 1.5 represents one and a half seconds.)
timeDeltaFromFracSecs :: (Real a) => a -> TimeDelta
timeDeltaFromFracSecs d =
  if d >= 0
    then TimeDelta SignPos (round (1000000000 * toRational d))
    else TimeDelta SignNeg (round (1000000000 * toRational (negate d)))

-- | Return a 'TimeDelta' corresponding the the given number of nanoseconds.
-- (For example, 1000000000 represends one second.)
timeDeltaFromNanos :: (Integral a) => a -> TimeDelta
timeDeltaFromNanos = fromIntegral

-- private
timeDeltaFromDiff :: Word64 -> Word64 -> TimeDelta
timeDeltaFromDiff end start =
  if end >= start
    then TimeDelta SignPos (end - start)
    else TimeDelta SignNeg (start - end)

-- private
timeDeltaAdd :: Word64 -> TimeDelta -> Word64
timeDeltaAdd t (TimeDelta s m) =
  case s of
    SignPos -> t + m
    SignNeg -> t - m

timeDeltaToFracSecs :: (Fractional a) => TimeDelta -> a
timeDeltaToFracSecs (TimeDelta s m) =
  let a = fromIntegral m / 1000000000
  in  case s of
        SignPos -> a
        SignNeg -> negate a

timeDeltaToNanos :: TimeDelta -> Maybe Word64
timeDeltaToNanos (TimeDelta s m) =
  case s of
    SignNeg -> Nothing
    SignPos -> Just m

threadDelayDelta :: TimeDelta -> IO ()
threadDelayDelta (TimeDelta s m) =
  case s of
    SignNeg -> pure ()
    SignPos -> threadDelay (fromIntegral (div m 1000))

class (Ord t) => TimeLike t where
  -- | `diffTime end start` computes `end - start`
  diffTime :: t -> t -> TimeDelta

  -- | `addTime start (diffTime end start) == end`
  addTime :: t -> TimeDelta -> t

  currentTime :: IO t

awaitDelta :: (TimeLike t) => t -> TimeDelta -> IO t
awaitDelta m t = do
  let target = addTime m t
  cur <- currentTime
  let td = diffTime target cur
  target <$ threadDelayDelta td

newtype PosixTime = PosixTime {unPosixTime :: Word64}
  deriving stock (Eq, Show, Ord, Bounded)

-- private
e9W :: Word64
e9W = 1000000000

-- private
picoToNanoWord :: Pico -> Word64
picoToNanoWord (MkFixed i) = fromInteger (div i 1000)

instance TimeLike PosixTime where
  diffTime (PosixTime t2) (PosixTime t1) = timeDeltaFromDiff t2 t1
  addTime (PosixTime t) td = PosixTime (timeDeltaAdd t td)
  currentTime = fmap (PosixTime . picoToNanoWord . nominalDiffTimeToSeconds) getPOSIXTime

-- | Monotonic time in nanoseconds since some unspecified epoch (see 'getMonotonicTimeNs')
newtype MonoTime = MonoTime {unMonoTime :: Word64}
  deriving stock (Eq, Show, Ord, Bounded)

monoTimeFromFracSecs :: (Real a, Show a) => a -> MonoTime
monoTimeFromFracSecs d = MonoTime (round (1000000000 * toRational (assertingNonNegative d)))

monoTimeFromNanos :: (Integral a, Show a) => a -> MonoTime
monoTimeFromNanos = MonoTime . fromIntegral . assertingNonNegative

monoTimeToFracSecs :: (Fractional a) => MonoTime -> a
monoTimeToFracSecs (MonoTime n) = fromIntegral n / 1000000000

monoTimeToNanos :: MonoTime -> Word64
monoTimeToNanos = unMonoTime

instance TimeLike MonoTime where
  diffTime (MonoTime t2) (MonoTime t1) = timeDeltaFromDiff t2 t1
  addTime (MonoTime t) td = MonoTime (timeDeltaAdd t td)
  currentTime = fmap MonoTime getMonotonicTimeNSec

newtype NtpTime = NtpTime {unNtpTime :: Word64}
  deriving stock (Eq, Show, Ord, Bounded)

-- private
nanoWordToSplit :: Word64 -> (Word32, Word32)
nanoWordToSplit j =
  let whole = div j e9W
      part = j - e9W * whole
  in  (fromIntegral whole, fromIntegral part)

-- private
nanoWordFromSplit :: Word32 -> Word32 -> Word64
nanoWordFromSplit whole part = e9W * fromIntegral whole + fromIntegral part

-- private
ntpFromSplit :: Word32 -> Word32 -> NtpTime
ntpFromSplit whole part = NtpTime (shiftL (fromIntegral whole) 32 .|. fromIntegral part)

-- private
ntpToSplit :: NtpTime -> (Word32, Word32)
ntpToSplit (NtpTime k) = (fromIntegral (shiftR k 32), fromIntegral k)

-- private
-- Difference in nano seconds between 1/1/1900 and 1/1/1970
-- 1900 is the NTP epoch, 1970 is the unix epoch
ntpEpochDiffSeconds :: Word32
ntpEpochDiffSeconds = 2208988800

-- | Convert 'NtpTime' to 'PosixTime'
posixToNtp :: PosixTime -> NtpTime
posixToNtp (PosixTime j) =
  let (whole, part) = nanoWordToSplit j
      whole' = whole + ntpEpochDiffSeconds
  in  ntpFromSplit whole' part

-- | Convert 'NtpTime' to 'PosixTime'
ntpToPosix :: NtpTime -> PosixTime
ntpToPosix k =
  let (whole, part) = ntpToSplit k
      whole' = whole - ntpEpochDiffSeconds
  in  PosixTime (nanoWordFromSplit whole' part)

-- (Probably best to do time arithmetic directly on PosixTime)
instance TimeLike NtpTime where
  diffTime n2 n1 = diffTime (ntpToPosix n2) (ntpToPosix n1)
  addTime n d = posixToNtp (addTime (ntpToPosix n) d)
  currentTime = fmap posixToNtp currentTime

-- private
assertingNonNegative :: (HasCallStack, Ord a, Num a, Show a) => a -> a
assertingNonNegative a =
  if a < 0
    then error ("Required non-negative value but got " ++ show a)
    else a
