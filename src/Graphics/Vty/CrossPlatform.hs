{-# LANGUAGE CPP #-}
-- | This module exposes a simple API to initialize Vty in a
-- platform-independent way. If you need access to Vty internals,
-- use the API from the @vty@ package. If you need access to
-- platform-specific settings, it might be best to depend on and use the
-- platform-specific packages directly.
module Graphics.Vty.CrossPlatform
  ( mkVty
  )
where

import Graphics.Vty (Vty)
import Graphics.Vty.Config (VtyUserConfig)

-- Import the platform-specific module that provides 'mkVty'. The
-- convention is that each platform package should export a 'mkVty' with
-- the type signature indicated below, so only branching at import time
-- like this ensures that the build will break if either 1) a platform
-- doesn't provide 'mkVty' or 2) provides it but at a different type.
--
-- This approach works fine for now with just two main platforms as
-- options, but if we need to support a third, we'll probably need to
-- make this check more sophisticated.
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Graphics.Vty.Platform.Windows as Platform
#else
import qualified Graphics.Vty.Platform.Unix as Platform
#endif

-- | Build a 'Vty' handle with the specified configuration.
--
-- This dispatches to the appropriate platform-specific implementation
-- at build time based on the build environment.
mkVty :: VtyUserConfig -> IO Vty
mkVty = Platform.mkVty
