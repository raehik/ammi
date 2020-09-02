  * All instances of `fromIntegral :: Integral a => a -> Word8` must come with a
    CLEAR <=255 guarantee
    * preferably this is provided by whatever function gives it to us, we
      shouldn't need to length check it at the point of `fromIntegral`
  * text-icu appears to convert `〜` full-width tilde to `FC FC` instead of `81
    60` -- which appears to mean, it doesn't recognise it. fuck uuuuuu
