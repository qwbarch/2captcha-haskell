module TwoCaptcha.Internal.Types.Captcha where

import Control.Lens (Iso', Lens', iso, lens, (&), (.~), (^.))
import Data.Text (Text, pack, unpack)
import GHC.Base (Coercible, coerce)
import Network.Wreq (Options, param)

-- | Creates a lens using the given field name.
lens' :: Coercible Options a => Text -> Lens' a (Maybe Text)
lens' field = lens getter setter
  where
    getter a =
      if null value
        then Nothing
        else Just $ head value
      where
        value = coerce a ^. param field
    setter a (Just value) = coerce $ coerce a & param field .~ [value]
    setter a Nothing = a

-- | Create a lens using the given field name for type __b__ with a 'Show' and 'Read' instance.
lens'' :: (Coercible Options a, Show b, Read b) => Text -> Lens' a (Maybe b)
lens'' field = lens' field . iso (read . unpack <$>) (pack . show <$>)

-- | Lenses for constructing captcha options.
class Coercible Options a => CaptchaLike a where
  -- | Your 2captcha API <https://2captcha.com/2captcha-api#solving_captchas key>.
  apiKey :: Lens' a (Maybe Text)
  apiKey = lens' "key"

  -- | Software developer id. Developers who integrate their software with 2captcha earn 10% of the user's spendings.
  softId :: Lens' a (Maybe Int)
  softId = lens'' "soft_id"

  -- | URL for <https://2captcha.com/2captcha-api#pingback pingback> (callback) response that will be sent the answer to when the captcha is solved.
  pingback :: Lens' a (Maybe Text)
  pingback = lens' "pingback"

  -- |
  -- Proxy to be sent to the worker who solves the captcha. You can read more about proxies <https://2captcha.com/2captcha-api#proxies here>.
  --
  -- Format must be in __login:password@123.123.123.123:3128__ .
  proxy :: Lens' a (Maybe Text)
  proxy = lens' "proxy"

  -- | Type of your proxy: __HTTP__, __HTTPS__, __SOCKS4__, __SOCKS5__.
  proxyType :: Lens' a (Maybe Text)
  proxyType = lens' "proxytype"
