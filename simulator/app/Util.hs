{-# LANGUAGE FlexibleContexts, TemplateHaskell, ScopedTypeVariables, QuasiQuotes, OverloadedStrings, NamedFieldPuns #-}

module Util where

import Data.Aeson as A
import Data.Binary.Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.String.Interpolate
import Network.HTTP
import Network.URI.Encode as URI
import Web.Cookie

roundTo n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)

cookiesToString = BL8.unpack . toLazyByteString . renderCookies

hitBackend user = do
  let req@(Request {rqHeaders}) = getRequest "http://34.212.78.41"
  let cookieHeader = Header HdrCookie (cookiesToString [("user", BS8.pack $ URI.encode $ BL8.unpack $ A.encode user)])
  let reqWithCookie = req { rqHeaders = cookieHeader : rqHeaders }
  response <- simpleHTTP reqWithCookie
  case response of
    Left err -> error [i|Failed to hit backend: #{err}|]
    Right (Response {rspCode=(2, 0, 0), rspBody}) -> return rspBody
    Right (Response {rspCode, rspBody}) -> error [i|Unexpected HTTP code: #{rspCode}, #{rspBody}|]

dummyHTML = [i|

<html>
<head>
	<link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400' rel='stylesheet' type='text/css'>
	<style type="text/css">
	body{ margin: 0; font: 14px/19px 'Helvetica Neue',Helvetica,sans-serif; color: #222;}
	p{
		line-height: 20px;
    	font-size: 14px;
    	margin: 1em 0;
    	font: 14px/19px 'Helvetica Neue',Helvetica,sans-serif;
    	color: #222;
	}
	strong{
		font-size:14px;
	}
	a{
		color: #0088FF;
	}
	a:visited{
		color: #15c;
	}

	</style>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
</head>

<body style="margin: 0; font-family: arial,sans-serif; color: #222;">

<div class="item" data-item-id="id1" data-item-category="cat1">contents1</div>
<div class="item" data-item-id="id2" data-item-category="cat2">contents2</div>

</body>
</html>

|]
