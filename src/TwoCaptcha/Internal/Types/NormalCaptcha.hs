module TwoCaptcha.Internal.Types.NormalCaptcha where

import Control.Lens (Lens')
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, HasLanguage, HasLocalImage, defaultCaptcha, mkParamLens', mkParamLensBool)

-- | Parameters used to solve a normal captcha.
newtype NormalCaptcha = MkNormalCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses NormalCaptcha

instance HasCaptchaLenses NormalCaptcha

instance HasLocalImage NormalCaptcha

instance HasLanguage NormalCaptcha

-- |
-- Parameters for solving a normal captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.method'
-- * 'TwoCaptcha.Internal.Types.Captcha.file'*
-- * 'TwoCaptcha.Internal.Types.Captcha.body'*
--
-- Optional parameters:
--
-- * 'phrase'
-- * 'caseSensitive'
-- * 'format'
-- * 'calc'
-- * 'minLength'
-- * 'maxLength'
-- * 'TwoCaptcha.Internal.Types.Captcha.language'
-- * 'TwoCaptcha.Internal.Types.Captcha.languageCode'
-- * 'TwoCaptcha.Internal.Types.Captcha.textInstructions'
-- * 'TwoCaptcha.Internal.Types.Captcha.imgInstructions'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
--
-- Possible 'TwoCaptcha.Internal.Types.Captcha.method' values:
--
-- * __post__ - defines that you're sending an image with multipart form
-- * __base64__  - defines that you're sending a base64 encoded image
--
-- Starred required parameter rules:
--
-- * __file__ is only required if __captcha = "post"__
-- * __body__ is only required if __captcha = "base64"__
normalCaptcha :: NormalCaptcha
normalCaptcha = MkNormalCaptcha defaultCaptcha

-- |
-- Defines if the captcha is a phrase.
--
-- * True - Captcha contains two or more words.
-- * False - Captcha contains one word.
phrase :: Lens' NormalCaptcha (Maybe Bool)
phrase = mkParamLensBool "phrase"

-- | Defines if the captcha is case sensitive.
caseSensitive :: Lens' NormalCaptcha (Maybe Bool)
caseSensitive = mkParamLensBool "regsense"

-- |
-- The captcha's format:
--
-- 0. Not specified.
-- 1. Captcha contains only numbers.
-- 2. Captcha contains only letters.
-- 3. Captcha contains only numbers OR only letters.
-- 4. Captcha contains both numbers AND letters.
format :: Lens' NormalCaptcha (Maybe Int)
format = mkParamLens' "numeric"

-- | Define if the captcha requires calculation (e.g. 1 + 1 = ?).
calc :: Lens' NormalCaptcha (Maybe Bool)
calc = mkParamLensBool "calc"

-- | The minimum number of symbols in the captcha (up to 20).
minLength :: Lens' NormalCaptcha (Maybe Int)
minLength = mkParamLens' "min_len"

-- | The maximum numbers of symbols in the captcha (up to 20).
maxLength :: Lens' NormalCaptcha (Maybe Int)
maxLength = mkParamLens' "max_len"
