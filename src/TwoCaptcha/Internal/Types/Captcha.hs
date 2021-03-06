module TwoCaptcha.Internal.Types.Captcha where

import Control.Lens (Getter, Lens', iso, lens, to, (%~), (&), (.~), (^.))
import Control.Lens.TH (makeLenses)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import GHC.Base (Coercible, coerce)
import Network.Wreq (Options, Part, defaults, param, partFile, partText)

-- import Prelude hiding (lookup)

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

-- | Represents the request information required to solve a captcha.
data Captcha = MkCaptcha
  { _options :: Options,
    _partTexts :: [(Text, Text)],
    _partFiles :: [(Text, FilePath)]
  }
  deriving (Show)

makeLenses ''Captcha

-- | Convert the captcha's multipart form parameters into a ['Part'].
parts :: Getter Captcha [Part]
parts = to (\captcha -> (uncurry partText <$> captcha ^. partTexts) ++ (uncurry partFile <$> captcha ^. partFiles))

instance HasCommonCaptchaLenses Captcha

instance HasCaptchaLenses Captcha

-- | Default parameters for solving a captcha. Internal use only.
defaultCaptcha :: Captcha
defaultCaptcha = MkCaptcha (defaults & param "json" .~ ["1"]) [] []

-- | Create a lens using the given field name for multipart forms.
mkPartLens :: (Coercible Captcha a, IsString s) => Lens' Captcha [(Text, s)] -> Text -> Lens' a (Maybe s)
mkPartLens partLens field = lens getter setter
  where
    getter a = lookup field (coerce a ^. partLens)
    setter a (Just value) = coerce $ coerce a & partLens %~ ((field, value) :)
    setter a Nothing = a

-- | Create a lens using the given field name for multipart form texts.
mkPartTextLens :: Coercible Captcha a => Text -> Lens' a (Maybe Text)
mkPartTextLens = mkPartLens partTexts

-- | Create a lens using the given field name for multipart form files.
mkPartFileLens :: Coercible Captcha a => Text -> Lens' a (Maybe FilePath)
mkPartFileLens = mkPartLens partFiles

-- | Creates a lens using the given field name for query parameters.
mkParamLens :: Coercible Captcha a => Text -> Lens' a (Maybe Text)
mkParamLens field = lens getter setter
  where
    getter a =
      let value = coerce a ^. options . param field
       in if null value then Nothing else Just $ head value
    setter a (Just value) = coerce $ coerce a & options %~ (& param field .~ [value])
    setter a Nothing = a

-- |
-- Create a lens using the given field name for type __b__ with a 'Show' and 'Read' instance.
--
-- GOTCHA: Bool values translate to 'True' or 'False'. Use 'mkLensBool' instead for bool lenses.
mkParamLens' :: (Coercible Captcha a, Show b, Read b) => Text -> Lens' a (Maybe b)
mkParamLens' field = mkParamLens field . iso (read . unpack <$>) (pack . show <$>)

-- |
-- Create a lens using the given field name for bools.
--
-- The boolean values become:
--
-- * 'True' - 1
-- * 'False' - 0
mkParamLensBool :: Coercible Captcha a => Text -> Lens' a (Maybe Bool)
mkParamLensBool field = mkParamLens field . iso (textToBool <$>) (boolToText <$>)
  where
    textToBool "0" = False
    textToBool "1" = True
    textToBool number = error $ unpack number <> " is not a valid bool value. This should never occur, please don't manipulate the options manually."
    boolToText False = "0"
    boolToText True = "1"

-- | Lenses for constructing options for 'TwoCaptcha.Internal.Client.submit'.
class Coercible Captcha a => HasCaptchaLenses a where
  -- | Software developer id. Developers who integrate their software with 2captcha earn 10% of the user's spendings.
  softId :: Lens' a (Maybe Int)
  softId = mkParamLens' "soft_id"

  -- | URL for <https://2captcha.com/2captcha-api#pingback pingback> (callback) response that will be sent the answer to when the captcha is solved.
  pingback :: Lens' a (Maybe Text)
  pingback = mkParamLens "pingback"

  -- | Type of captcha to solve.
  method :: Lens' a (Maybe Text)
  method = mkParamLens "method"

-- | Lenses for constructing options for 'TwoCaptcha.Internal.Client.submit' and 'TwoCaptcha.Internal.Client.answer'.
class Coercible Captcha a => HasCommonCaptchaLenses a where
  -- | Your 2captcha API <https://2captcha.com/2captcha-api#solving_captchas key>.
  apiKey :: Lens' a (Maybe Text)
  apiKey = mkParamLens "key"

  -- |
  -- If True, 'TwoCaptcha.Internal.Client.submit' will include the __Access-Control-Allow-Origin:*__ header in the response.
  -- Used for cross-domain AJAX requests in web applications.
  headerACAO :: Lens' a (Maybe Bool)
  headerACAO = mkParamLensBool "header_acao"

-- | Parameters used to retrieve the 'TwpCaptcha.Internal.Client.answer' of a solved captcha.
newtype CaptchaRes = CaptchaRes Captcha deriving (Show)

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
captchaRes :: CaptchaRes
captchaRes = CaptchaRes (defaultCaptcha & options %~ (& param "action" .~ ["get"]))

-- | The captcha id returned from 'TwoCaptcha.Internal.Client.submit'.
captchaId :: Lens' CaptchaRes (Maybe Text)
captchaId = mkParamLens "id"

class Coercible Captcha a => HasPageURL a where
  -- | Full URL of the page where the captcha is found.
  pageUrl :: Lens' a (Maybe Text)
  pageUrl = mkParamLens "pageurl"

class Coercible Captcha a => HasProxy a where
  -- |
  -- Proxy to be sent to the worker who solves the captcha. You can read more about proxies <https://2captcha.com/2captcha-api#proxies here>.
  --
  -- Format must be in __login:password@123.123.123.123:3128__ .
  proxy :: Lens' a (Maybe Text)
  proxy = mkParamLens "proxy"

  -- | Type of your proxy: __HTTP__, __HTTPS__, __SOCKS4__, __SOCKS5__.
  proxyType :: Lens' a (Maybe Text)
  proxyType = mkParamLens "proxytype"

class Coercible Captcha a => HasLocalImage a where
  -- | File path of a captcha image.
  file :: Lens' a (Maybe FilePath)
  file = mkPartFileLens "file"

  -- | Base-64 encoded image.
  body :: Lens' a (Maybe Text)
  body = mkPartTextLens "body"

  -- | Text which is shown to the worker to help solve a captcha.
  textInstructions :: Lens' a (Maybe Text)
  textInstructions = mkParamLens "textInstructions"

  -- | Image file path with instructions on solving a captcha.
  imgInstructions :: Lens' a (Maybe FilePath)
  imgInstructions = mkPartFileLens "imgInstructions"

class Coercible Captcha a => HasLanguage a where
  -- |
  -- The captcha's language:
  --
  -- 0. Not specified.
  -- 1. Cyrillic captcha.
  -- 2. Latin captcha.
  language :: Lens' a (Maybe Int)
  language = mkParamLens' "language"

  -- | The captcha's language code. Click <https://2captcha.com/2captcha-api#language here> for a list of supported languages.
  languageCode :: Lens' a (Maybe Text)
  languageCode = mkParamLens "lang"

class Coercible Captcha a => HasUserAgent a where
  -- | User agent that will be used by the worker when solving the captcha.
  userAgent :: Lens' a (Maybe Text)
  userAgent = mkParamLens "userAgent"

class Coercible Captcha a => HasCookies a where
  -- |
  -- Cookies that will be used by the worker solving the captcha. The used cookies will also be included in the response.
  --
  -- Format: __KEY1:Value1;KEY2:Value2;__
  cookies :: Lens' a (Maybe Text)
  cookies = mkParamLens "cookies"
