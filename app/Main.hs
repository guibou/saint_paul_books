{-# LANGUAGE CPP #-}

#if !defined(javascript_HOST_ARCH)
import Network.WebSockets
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
#endif
import Language.Javascript.JSaddle.Warp
import Android (widget)

#if !defined(javascript_HOST_ARCH)
main :: IO ()
main = do
  app <-
    jsaddleWithAppOr defaultConnectionOptions widget
      $ staticApp
      $ defaultWebAppSettings "static/"
  runSettings (setPort 8080 (setTimeout 3600 Network.Wai.Handler.Warp.defaultSettings)) app
#else
main = widget
#endif
