module TwoCaptcha.Internal.Types.Proxy where

import Data.Text (Text)

-- | The possible types of proxies.
data ProxyType = HTTP | HTTPS | SOCKS4 | SOCKS5 deriving (Show)

-- | Authentication details of a proxy.
data ProxyAuth = ProxyAuth
  { user :: Text,
    password :: Text
  }
  deriving (Show)

-- | Required information to use a proxy.
data Proxy = Proxy
  { address :: Text,
    port :: Int,
    authentication :: Maybe ProxyAuth
  }
  deriving (Show)
