# 2captcha-haskell

[![Hackage](http://img.shields.io/hackage/v/2captcha.svg)](https://hackage.haskell.org/package/2captcha)

Note: This package is deprecated by [captcha-haskell](https://github.com/qwbarch/captcha-haskell).

The easiest way to integrate 2captcha into your application to automate common types of captchas.

#### Table of Contents
- [Solving a captcha](#solving-a-captcha)
  - [Normal Captcha](#normal-captcha)
  - [Text Captcha](#text-captcha)
  - [ReCaptcha V2](#recaptcha-v2)
  - [ReCaptcha V3](#recaptcha-v3)
  - [FunCaptcha](#funcaptcha)
  - [GeeTest](#geetest)
  - [hCaptcha](#hcaptcha)
  - [KeyCaptcha](#keycaptcha)
  - [Capy](#capy)
  - [Grid](#grid)
  - [Coordinate](#coordinate)
  - [Rotate](#rotate)
  - [TikTok](#tiktok)

## Solving a captcha

Solving a captcha using 2captcha's api is through the ``in.php`` and ``res.php`` endpoints.  
This package encapsulates the mentioned endpoints through [submit](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Client.hs#L48) and [answer](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Client.hs#L51).  
If you'd like to poll for the answer, use [solve](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Client.hs#L54).

[Default timeout](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Types/Captcha.hs#L22): 120 seconds.  
[ReCAPTCHA timeout](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Types/ReCaptcha.hs#L118): 600 seconds.  
[Polling interval](https://github.com/qwbarch/2captcha-haskell/blob/e442950a79f1aef7fd2fb95aac9cd9bfe41a8df7/src/TwoCaptcha/Internal/Types/Captcha.hs#L26): 10 seconds

Below are the minimal required parameters to solve each type of captcha.

### Normal Captcha

This function can be used to solve a local image captcha.

```haskell
let captcha =
      normalCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & method ?~ "post"
        & file ?~ "/path/to/file"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

This function can be used to solve a captcha encoded in base-64 form.

```haskell
let captcha =
  normalCaptcha
    & apiKey ?~ "YOUR_API_KEY"
    & method ?~ "base64"
    & body ?~ "BASE_64_ENCODED_IMAGE"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### Text Captcha

This function can be used to solve captchas in text form.

```haskell
let captcha =
      textCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & textContent ?~ "If tomorrow is Saturday, what day is it today?"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### ReCAPTCHA v2

ReCAPTCHA v2 requires a ``k`` or ``data-sitekey`` value found on the captcha page.

```haskell
let captcha =
      reCAPTCHAV2
        & apiKey ?~ "YOUR_API_KEY"
        & googleKey ?~ "GOOGLE_KEY_VALUE"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval reCAPTCHATimeout
```

### ReCaptcha v3

ReCAPTCHA v3 requires a ``k`` or ``data-sitekey`` value found on the captcha page.

```haskell
let captcha =
      reCAPTCHAV3
        & apiKey ?~ "YOUR_API_KEY"
        & googleKey ?~ "GOOGLE_KEY_VALUE"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval reCAPTCHATimeout
```

### FunCaptcha

FunCaptcha (ArkoseLabs) requires a ``pk`` or ``data-pkey`` value found on the captcha page.

```haskell
let captcha =
      funCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & publicKey ?~ "PUBLIC_KEY"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### GeeTest

GeeTest requires the ``gt``, ``challenge``, and ``api_server`` values on the captcha page.

```haskell
let captcha =
      geeTestCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & gt ?~ "GT_VALUE"
        & apiServer ?~ "API_SERVER"
        & challenge ?~ "CAPTCHA_CHALLENGE"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### hCaptcha

hCaptcha requires the ``data-sitekey`` value on the captcha page.

```haskell
let captcha =
      hCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & siteKey ?~ "SITE_KEY"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### KeyCaptcha

KeyCaptcha requires the ``s_s_c_user_id``, ``s_s_c_session_id``, ``s_s_c_web_server_sign``, and ``s_s_c_web_server_sign2`` values on the captcha page.

```haskell
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
```

### Capy

Capy requires the ``captchakey`` value on the captcha page.

```haskell
let captcha =
      capyCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & captchaKey ?~ "CAPTCHA_KEY"
        & pageUrl ?~ "PAGE_URL"
        & scriptDomain ?~ "SCRIPT_DOMAIN"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### Grid

This function can be used to solve a local grid image captcha.

```haskell
let captcha =
      gridCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & method ?~ "post"
        & file ?~ "/path/to/file"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### Coordinate

This function can be used to solve a local coordinate image captcha.

```haskell
let captcha =
      coordinateCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & method ?~ "post"
        & file ?~ "/path/to/file"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### Rotate

This function can be used to solve a local rotate image captcha.

```haskell
let captcha =
      rotateCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & file ?~ "/path/to/file"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```

### TikTok

TikTok requires the ``aid`` and ``host`` values on the captcha page.

```haskell
let captcha =
      tikTokCaptcha
        & apiKey ?~ "YOUR_API_KEY"
        & cookies ?~ "YOUR_COOKIES"
        & aid ?~ 0
        & host ?~ "HOST_VALUE"
        & pageUrl ?~ "PAGE_URL"
session <- newAPISession
solve session captcha pollingInterval captchaTimeout
```


