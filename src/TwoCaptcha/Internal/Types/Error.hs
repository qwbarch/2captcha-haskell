module TwoCaptcha.Internal.Types.Error where

import Data.Functor (($>))
import Text.Parsec (ParseError, parse, try, (<|>))
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser)

-- | Possible errors when using the 2captcha API.
data TwoCaptchaError
  = -- | The api key you provided is invalid. Please ensure it is 32 characters long.
    WrongUserKey
  | -- | The key you've provided does not exist.
    KeyDoesNotExist
  | -- | You don't have funds in your account.
    ZeroBalance
  | -- | The __pageurl__ parameter is missing in your request.
    PageUrlMissing
  | -- |
    -- You can receive this error in two cases:
    --
    -- 1. __If you solve token-based captchas (reCAPTCHA, hCaptcha, ArkoseLabs FunCaptcha, GeeTest, etc):__
    -- the queue of your captchas that are not distributed to workers is too long.
    -- Queue limit changes dynamically and depends on total amount of captchas awaiting solution and usually it’s between 50 and 100 captchas.
    --
    -- 2. __If you solve Normal Captcha:__ your maximum rate for normal captchas is lower than current rate on the server.
    -- You can change your maximum rate in <https://2captcha.com/setting your account's settings.>
    NoSlotAvailable
  | -- | Image size is less than 100 bytes.
    ZeroCaptchaFileSize
  | -- | Image size is more than 100 kB.
    TooBigCaptchaFileSize
  | -- | Image file has unsupported extension. Accepted extensions: jpg, jpeg, gif, png.
    WrongFileExtension
  | -- | Server can't recognize image file type.
    ImageTypeNotSupported
  | -- |
    -- Server can't get file data from your POST-request.
    -- That happens if your POST-request is malformed or base64 data is not a valid base64 image.
    UploadFailure
  | -- | The request is sent from the IP that is not on the list of your allowed IPs.
    IpNotAllowed
  | -- | Your IP address is banned due to many frequent attempts to access the server using wrong authorization keys.
    IpBanned
  | -- |
    -- You can get this error code when sending reCAPTCHA V2. This happens if your request contains invalid pair of googlekey and pageurl.
    -- The common reason for that is that reCAPTCHA is loaded inside an iframe hosted on another domain/subdomain.
    BadTokenOrPageUrl
  | -- | You can get this error code when sending reCAPTCHA V2. That means that sitekey value provided in your request is incorrect: it's blank or malformed.
    GoogleKeyInvalid
  | -- | The __googlekey__ parameter is missing in your request.
    GoogleKeyMissing
  | -- |
    -- You've sent an image that is marked in 2captcha's database as unrecognizable.
    -- Usually that happens if the website where you found the captcha stopped sending you captchas and started to send a "deny access" image.
    CaptchaImageBlocked
  | -- | You are sending too many unrecognizable images.
    TooManyBadImages
  | -- |
    -- You made more than 60 requests to in.php within 3 seconds.
    -- Your account is banned for 10 seconds. Ban will be lifted automatically.
    RateLimited
  | -- |
    -- You received the error 'NoSlotAvailable' 120 times in one minute because your current bid is lower than current bid on the server.
    --
    -- Blocking time: 10 minutes.
    Error1001
  | -- |
    -- You received the error 'ZeroBalance' 120 times in one minute because your balance is zero.
    --
    -- Blocking time: 5 minutes.
    Error1002
  | -- |
    -- You received the error 'NoSlotAvailable' because you are uploading many captchas and server has a long queue of your captchas that are not distributed to workers.
    -- You received three times more errors than amount of captchas you sent (but not less than 120 errors). Increase the timeout if you see this error.
    --
    -- Blocking time: 30 seconds.
    Error1003
  | -- | Your IP address is blocked because there were 5 requests with incorrect API key from your IP.
    Error1004
  | -- |
    -- You are making too many requests to res.php to get answers.
    --
    --
    -- 2captcha uses the following rule to block your account: R > C * 20 + 1200
    --
    -- Where:
    --
    -- * R - the amount of your requests
    --
    -- * C - the amount of captchas you've uploaded
    --
    -- That means that you don't have to make more than 20 requests to res.php per each captcha.
    -- Please remember that balance request sent to res.php also counts!
    --
    -- To get your answer faster without a risk to be blocked you can use <https://2captcha.com/2captcha-api#pingback pingback> feature and 2captcha will send you the answer when your captcha is solved.
    --
    -- Blocking time: 10 minutes.
    Error1005
  | -- |
    -- The error code is returned if some required parameters are missing in your request or the values have incorrect format.
    -- For example if you submit <https://2captcha.com/2captcha-api#grid Grid images> but your request is missing an instruction for workers.
    --
    -- Blocking time: 5 minutes.
    BadParameters
  | -- | You can get this error code when sending a captcha via proxy server which is marked as BAD by the 2captcha API.
    BadProxy
  | -- | Your captcha is not solved yet.
    CaptchaNotReady
  | -- |
    -- 2captcha was unable to solve your captcha - three of their workers were unable solve it or they didn't get an answer within 90 seconds (300 seconds for reCAPTCHA V2).
    --
    -- You will not be charged for that request.
    CaptchaUnsolvable
  | -- | You've provided captcha ID in wrong format. The ID can contain numbers only.
    WrongIdFormat
  | -- | You provided an invalid captcha id.
    WrongCaptchaId
  | -- | Error is returned when 100% accuracy feature is enabled. The error means that max numbers of tries is reached but min number of matches not found.
    BadDuplicates
  | -- |
    -- Error is returned to your <https://2captcha.com/2captcha-api#complain report> request if you already complained lots of correctly solved captchas (more than 40%).
    -- Or if more than 15 minutes passed after you submitted the captcha.
    ReportNotRecorded
  | -- | Error is returned to your <https://2captcha.com/2captcha-api#complain report request> if you are trying to report the same captcha more than once.
    DuplicateReport
  | -- |
    -- You can receive this error code when registering a <https://2captcha.com/2captcha-api#pingback pingback (callback)> IP or domain.
    --
    -- This happens if your request is coming from an IP address that doesn't match the IP address of your pingback IP or domain.
    InvalidPingbackIp
  | -- |
    -- You can receive this error code when sending <https://2captcha.com/2captcha-api#solving_geetest GeeTest>.
    -- This error means the __challenge__ value you provided is expired.
    TokenExpired
  | -- | Action parameter is missing or no value is provided for __action__ parameter.
    EmptyAction
  | -- |
    -- You can get this error code if we were unable to load a captcha through your proxy server.
    -- The proxy will be marked as BAD by our API and we will not accept requests with the proxy during 10 minutes.
    -- You will recieve ERROR_BAD_PROXY code from in.php API endpoint in such case.
    ProxyConnectionFailed
  deriving (Show, Eq)

-- | The raw error code provided by 2captcha.
errorCode :: TwoCaptchaError -> String
errorCode WrongUserKey = "ERROR_WRONG_USER_KEY"
errorCode KeyDoesNotExist = "ERROR_KEY_DOES_NOT_EXIST"
errorCode ZeroBalance = "ERROR_ZERO_BALANCE"
errorCode PageUrlMissing = "ERROR_PAGEURL"
errorCode NoSlotAvailable = "ERROR_NO_SLOT_AVAILABLE"
errorCode ZeroCaptchaFileSize = "ERROR_ZERO_CAPTCHA_FILESIZE"
errorCode TooBigCaptchaFileSize = "ERROR_TOO_BIG_CAPTCHA_FILESIZE"
errorCode WrongFileExtension = "ERROR_WRONG_FILE_EXTENSION"
errorCode ImageTypeNotSupported = "ERROR_IMAGE_TYPE_NOT_SUPPORTED"
errorCode UploadFailure = "ERROR_UPLOAD"
errorCode IpNotAllowed = "ERROR_IP_NOT_ALLOWED"
errorCode IpBanned = "IP_BANNED"
errorCode BadTokenOrPageUrl = "ERROR_BAD_TOKEN_OR_PAGEURL"
errorCode GoogleKeyInvalid = "ERROR_GOOGLEKEY"
errorCode GoogleKeyMissing = "ERROR_WRONG_GOOGLEKEY"
errorCode CaptchaImageBlocked = "ERROR_CAPTCHAIMAGE_BLOCKED"
errorCode TooManyBadImages = "TOO_MANY_BAD_IMAGES"
errorCode RateLimited = "MAX_USER_TURN"
errorCode Error1001 = "ERROR: 1001"
errorCode Error1002 = "ERROR: 1002"
errorCode Error1003 = "ERROR: 1003"
errorCode Error1004 = "ERROR: 1004"
errorCode Error1005 = "ERROR: 1005"
errorCode BadParameters = "ERROR_BAD_PARAMETERS"
errorCode BadProxy = "ERROR_BAD_PROXY"
errorCode CaptchaNotReady = "CAPCHA_NOT_READY"
errorCode CaptchaUnsolvable = "ERROR_CAPTCHA_UNSOLVABLE"
errorCode WrongIdFormat = "ERROR_WRONG_ID_FORMAT"
errorCode WrongCaptchaId = "ERROR_WRONG_CAPTCHA_ID"
errorCode BadDuplicates = "ERROR_BAD_DUPLICATES"
errorCode ReportNotRecorded = "ERROR_REPORT_NOT_RECORDED"
errorCode DuplicateReport = "ERROR_DUPLICATE_REPORT"
errorCode InvalidPingbackIp = "ERROR_IP_ADDRES"
errorCode TokenExpired = "ERROR_TOKEN_EXPIRED"
errorCode EmptyAction = "ERROR_EMPTY_ACTION"
errorCode ProxyConnectionFailed = "ERROR_PROXY_CONNECTION_FAILED"

-- | Parser instance for parsing an error code to a 'TwoCaptchaError'.
errorParser :: Parser TwoCaptchaError
errorParser =
  try (string (errorCode WrongUserKey) $> WrongUserKey)
    <|> try (string (errorCode KeyDoesNotExist) $> KeyDoesNotExist)
    <|> try (string (errorCode ZeroBalance) $> ZeroBalance)
    <|> try (string (errorCode PageUrlMissing) $> PageUrlMissing)
    <|> try (string (errorCode NoSlotAvailable) $> NoSlotAvailable)
    <|> try (string (errorCode ZeroCaptchaFileSize) $> ZeroCaptchaFileSize)
    <|> try (string (errorCode TooBigCaptchaFileSize) $> TooBigCaptchaFileSize)
    <|> try (string (errorCode WrongFileExtension) $> WrongFileExtension)
    <|> try (string (errorCode ImageTypeNotSupported) $> ImageTypeNotSupported)
    <|> try (string (errorCode UploadFailure) $> UploadFailure)
    <|> try (string (errorCode IpNotAllowed) $> IpNotAllowed)
    <|> try (string (errorCode IpBanned) $> IpBanned)
    <|> try (string (errorCode BadTokenOrPageUrl) $> BadTokenOrPageUrl)
    <|> try (string (errorCode GoogleKeyInvalid) $> GoogleKeyInvalid)
    <|> try (string (errorCode GoogleKeyMissing) $> GoogleKeyMissing)
    <|> try (string (errorCode CaptchaImageBlocked) $> CaptchaImageBlocked)
    <|> try (string (errorCode TooManyBadImages) $> TooManyBadImages)
    <|> try (string (errorCode RateLimited) $> RateLimited)
    <|> try (string (errorCode Error1001) $> Error1001)
    <|> try (string (errorCode Error1002) $> Error1002)
    <|> try (string (errorCode Error1003) $> Error1003)
    <|> try (string (errorCode Error1004) $> Error1004)
    <|> try (string (errorCode Error1005) $> Error1005)
    <|> try (string (errorCode BadParameters) $> BadParameters)
    <|> try (string (errorCode BadProxy) $> BadProxy)
    <|> try (string (errorCode CaptchaNotReady) $> CaptchaNotReady)
    <|> try (string (errorCode CaptchaUnsolvable) $> CaptchaUnsolvable)
    <|> try (string (errorCode WrongIdFormat) $> WrongIdFormat)
    <|> try (string (errorCode WrongCaptchaId) $> WrongCaptchaId)
    <|> try (string (errorCode BadDuplicates) $> BadDuplicates)
    <|> try (string (errorCode ReportNotRecorded) $> ReportNotRecorded)
    <|> try (string (errorCode DuplicateReport) $> DuplicateReport)
    <|> try (string (errorCode InvalidPingbackIp) $> InvalidPingbackIp)
    <|> try (string (errorCode TokenExpired) $> TokenExpired $> TokenExpired $> TokenExpired $> TokenExpired)
    <|> try (string (errorCode EmptyAction) $> EmptyAction)
    <|> try (string (errorCode ProxyConnectionFailed) $> ProxyConnectionFailed)

-- | Read an error code as its corresponding 'TwoCaptchaError'.
readErrorCode :: String -> Either ParseError TwoCaptchaError
readErrorCode = parse errorParser ""
