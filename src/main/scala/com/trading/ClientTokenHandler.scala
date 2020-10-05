package com.trading

import com.pangility.schwab.api.client.oauth2.{SchwabAccount, SchwabTokenHandler}

class ClientTokenHandler extends SchwabTokenHandler {
  @Override
  def onAccessTokenChange(schwabAccount: SchwabAccount): Unit = {
    // Do something with the Access Token here
    val accessToken = schwabAccount.getAccessToken
    println(s"Access token changed for user: ${schwabAccount.getUserId}. New access token: $accessToken")
    //TODO: Save token here.
  }

  @Override
  def onRefreshTokenChange(schwabAccount: SchwabAccount): Unit = {
    // Save refresh token
    val refreshToken = schwabAccount.getRefreshToken
    val refreshExpiration = schwabAccount.getRefreshExpiration
    println(s"Refresh Token: $refreshToken")
    println(s"Expires on: $refreshExpiration")
    //TODO: Save token here.
  }
}



