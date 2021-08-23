module TwoCaptcha.Internal.Types.Error where

data SubmitError
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
