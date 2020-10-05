package com.trading

import org.springframework.stereotype.Service

@Service
class TokenGenerator {
  def generateAuthorizationUrl(
                                schwabUserId: String,
                                callbackUrl: String
                              ): String = {
    s"https://localhost:8080/oauth2/schwab/authorization?schwabUserId=$schwabUserId&callback=$callbackUrl"
  }
}

// Usage example
object TokenGenerationApp extends App {
  val tokenGenerator = new TokenGenerator()

  val schwabUserId = sys.env.getOrElse("SCHWAB_USER_ID", "frankpalardy@hotmail.com")
  val callbackUrl = "https://127.0.0.1/oauth2/schwab/code"  // Or your specific callback URL

  val authorizationUrl = tokenGenerator.generateAuthorizationUrl(schwabUserId, callbackUrl)

  println(s"Open this URL in a browser: $authorizationUrl")
}