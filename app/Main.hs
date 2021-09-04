import Control.Lens ((&), (?~))
import Data.Text (Text)
import Network.Wreq.Session (newAPISession)
import TwoCaptcha.Captcha
  ( CapyCaptcha (MkCapyCaptcha),
    CoordinateCaptcha (MkCoordinateCaptcha),
    FunCaptcha (MkFunCaptcha),
    GeeTestCaptcha (MkGeeTestCaptcha),
    GridCaptcha (MkGridCaptcha),
    HCaptcha (MkHCaptcha),
    HasCaptchaLenses (method),
    HasCommonCaptchaLenses (apiKey),
    HasLanguage (languageCode),
    HasLocalImage (body, file, textInstructions),
    HasPageURL (pageUrl),
    HasReCaptchaLenses (googleKey),
    KeyCaptcha (MkKeyCaptcha),
    NormalCaptcha (MkNormalCaptcha),
    ReCaptchaV2 (MkReCaptchaV2),
    ReCaptchaV3 (MkReCaptchaV3),
    RotateCaptcha (MkRotateCaptcha),
    TextCaptcha (MkTextCaptcha),
    TwoCaptchaClient (solve),
    apiServer,
    captchaKey,
    captchaTimeout,
    capyCaptcha,
    challenge,
    coordinateCaptcha,
    funCaptcha,
    geeTestCaptcha,
    gridCaptcha,
    gt,
    hCaptcha,
    keyCaptcha,
    normalCaptcha,
    pollingInterval,
    publicKey,
    reCAPTCHAV2,
    reCAPTCHAV3,
    rotateCaptcha,
    scriptDomain,
    sessionId,
    siteKey,
    textCaptcha,
    textContent,
    userId,
    webServerSign,
    webServerSign2,
  )

main :: IO ()
main = textCaptchaExample >>= print

textCaptchaExample = do
  let captcha =
        textCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & textContent ?~ "If tomorrow is Saturday, what day is it today?"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

capyExample = do
  let captcha =
        capyCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & captchaKey ?~ "CAPTCHA_KEY"
          & pageUrl ?~ "PAGE_URL"
          & scriptDomain ?~ "SCRIPT_DOMAIN"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

coordinatesExample = do
  let captcha =
        coordinateCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & method ?~ "post"
          & file ?~ "/path/to/file"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

funCaptchaExample = do
  let captcha =
        funCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & publicKey ?~ "PUBLIC_KEY"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

geeTestExample = do
  let captcha =
        geeTestCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & gt ?~ "GT_VALUE"
          & apiServer ?~ "API_SERVER"
          & challenge ?~ "CAPTCHA_CHALLENGE"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

gridExample = do
  let captcha =
        gridCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & method ?~ "post"
          & file ?~ "/path/to/file"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

hCaptchaExample = do
  let captcha =
        hCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & siteKey ?~ "SITE_KEY"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

keyCaptchaExample = do
  let captcha =
        keyCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & userId ?~ "USER_ID_VALUE"
          & sessionId ?~ "SESSION_ID_VALUE"
          & webServerSign ?~ "WEB_SERVER_SIGN_VALUE"
          & webServerSign2 ?~ "WEB_SERVER_SIGN_2_VALUE"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

normalCaptchaExample = do
  let captcha =
        normalCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & method ?~ "post"
          & file ?~ "/path/to/file"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

reCAPTCHAV2Example = do
  let captcha =
        reCAPTCHAV2
          & apiKey ?~ "YOUR_API_KEY"
          & googleKey ?~ "GOOGLE_KEY_VALUE"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

reCAPTCHAV3Example = do
  let captcha =
        reCAPTCHAV3
          & apiKey ?~ "YOUR_API_KEY"
          & googleKey ?~ "GOOGLE_KEY_VALUE"
          & pageUrl ?~ "PAGE_URL"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout

rotateExample = do
  let captcha =
        rotateCaptcha
          & apiKey ?~ "YOUR_API_KEY"
          & file ?~ "/path/to/file"
  session <- newAPISession
  solve session captcha pollingInterval captchaTimeout
