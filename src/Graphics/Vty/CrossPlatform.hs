{-# LANGUAGE CPP #-}
module Graphics.Vty.CrossPlatform
  ( mkVty
  )
where

import Graphics.Vty (Vty)
import Graphics.Vty.Config (VtyUserConfig)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Graphics.Vty.Platform.Windows as Platform
#else
import qualified Graphics.Vty.Platform.Unix as Platform
#endif

mkVty :: VtyUserConfig -> IO Vty
mkVty = Platform.mkVty
