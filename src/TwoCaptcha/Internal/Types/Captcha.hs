module TwoCaptcha.Internal.Types.Captcha where

import Control.Lens (Lens', iso, lens, (&), (.~), (^.))
import Data.Text (Text, pack, unpack)
import GHC.Base (Coercible, coerce)
import Network.Wreq (Options, defaults, param)

-- | The id of a captcha being solved.
type CaptchaId = Text

-- | Time in milliseconds in how often to request the answer.
type PollingInterval = Int

-- | Time in milliseconds on when to timeout if the request takes too long.
type TimeoutDuration = Integer

-- | Default captcha timeout duration (120 seconds).
captchaTimeout :: TimeoutDuration
captchaTimeout = 120000

-- | Default polling interval (10 seconds).
pollingInterval :: PollingInterval
pollingInterval = 10000

-- | Defaults options for a captcha.
defaultCaptchaOptions :: Options
defaultCaptchaOptions = defaults & param "json" .~ ["1"]

-- | Creates a lens using the given field name.
mkLens :: Coercible Options a => Text -> Lens' a (Maybe Text)
mkLens field = lens getter setter
  where
    getter a =
      if null value
        then Nothing
        else Just $ head value
      where
        value = coerce a ^. param field
    setter a (Just value) = coerce $ coerce a & param field .~ [value]
    setter a Nothing = a

-- |
-- Create a lens using the given field name for type __b__ with a 'Show' and 'Read' instance.
--
-- GOTCHA: Bool values translate to 'True' or 'False'. Use 'mkLensBool' instead for bool lenses.
mkLens' :: (Coercible Options a, Show b, Read b) => Text -> Lens' a (Maybe b)
mkLens' field = mkLens field . iso (read . unpack <$>) (pack . show <$>)

-- |
-- Create a lens using the given field name for bools.
--
-- The boolean values become:
--
-- * 'True' - 1
-- * 'False' - 0
mkLensBool :: Coercible Options a => Text -> Lens' a (Maybe Bool)
mkLensBool field = mkLens field . iso (textToBool <$>) (boolToText <$>)
  where
    textToBool "0" = False
    textToBool "1" = True
    textToBool number = error $ unpack number <> " is not a valid bool value. This should never occur, please don't manipulate the options manually."
    boolToText False = "0"
    boolToText True = "1"

-- | Lenses for constructing options for 'TwoCaptcha.Internal.Client.submit'.
class Coercible Options a => HasCaptchaLenses a where
  -- | Software developer id. Developers who integrate their software with 2captcha earn 10% of the user's spendings.
  softId :: Lens' a (Maybe Int)
  softId = mkLens' "soft_id"

  -- | URL for <https://2captcha.com/2captcha-api#pingback pingback> (callback) response that will be sent the answer to when the captcha is solved.
  pingback :: Lens' a (Maybe Text)
  pingback = mkLens "pingback"

  -- |
  -- Proxy to be sent to the worker who solves the captcha. You can read more about proxies <https://2captcha.com/2captcha-api#proxies here>.
  --
  -- Format must be in __login:password@123.123.123.123:3128__ .
  proxy :: Lens' a (Maybe Text)
  proxy = mkLens "proxy"

  -- | Type of your proxy: __HTTP__, __HTTPS__, __SOCKS4__, __SOCKS5__.
  proxyType :: Lens' a (Maybe Text)
  proxyType = mkLens "proxytype"

-- | Lenses for constructing options for 'TwoCaptcha.Internal.Client.submit' and 'TwoCaptcha.Internal.Client.answer'.
class Coercible Options a => HasCommonCaptchaLenses a where
  -- | Your 2captcha API <https://2captcha.com/2captcha-api#solving_captchas key>.
  apiKey :: Lens' a (Maybe Text)
  apiKey = mkLens "key"

  -- |
  -- If True, 'TwoCaptcha.Internal.Client.submit' will include the __Access-Control-Allow-Origin:*__ header in the response.
  -- Used for cross-domain AJAX requests in web applications.
  headerACAO :: Lens' a (Maybe Bool)
  headerACAO = mkLensBool "header_acao"

-- | Parameters used to retrieve the 'TwpCaptcha.Internal.Client.answer' of a solved captcha.
newtype CaptchaRes = CaptchaRes Options deriving (Show)

instance HasCommonCaptchaLenses CaptchaRes

-- |
-- Parameters for retrieving a captcha's answer.
--
-- Required parameters:
--
-- * 'apiKey'
-- * 'captchaId'
--
-- Optional parameters:
--
-- * 'headerACAO'
defaultCaptchaRes :: CaptchaRes
defaultCaptchaRes = CaptchaRes defaults

-- | The captcha id returned from 'TwoCaptcha.Internal.Client.submit'.
captchaId :: Lens' CaptchaRes (Maybe Text)
captchaId = mkLens "id"
