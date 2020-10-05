package com.trading

import com.pangility.schwab.api.client.marketdata.{EnableSchwabMarketDataApi, SchwabMarketDataApiClient}
import com.pangility.schwab.api.client.oauth2.SchwabAccount
import org.springframework.beans.factory.annotation.{Autowired, Value}
import org.springframework.stereotype.Service

import java.time.{Duration, LocalDateTime}
import java.util

@Service
@EnableSchwabMarketDataApi
class SchwabOAuthManager @Autowired()(
                                       @Value("${schwab.userId}") userId: String,
                                     ) {

  @Autowired
  private val schwabMarketDataApiClient: SchwabMarketDataApiClient = new SchwabMarketDataApiClient()

  private val tokenHandler = new ClientTokenHandler

  def init(): Unit = {
    // Initialize the client with a basic account (no token yet)
    val currentTime = LocalDateTime.now()
    val refreshExpiration = currentTime.plus(Duration.ofMinutes(2))

    if (!schwabMarketDataApiClient.isInitialized) {
      val accounts = new util.ArrayList[SchwabAccount]()
      val account = new SchwabAccount()
      account.setUserId(userId)
      accounts.add(account)

      schwabMarketDataApiClient.init(account, tokenHandler)
    }
  }
}
/*

// Usage Example
object OAuthReactiveApp extends App {
  val oauthManager = new com.trading.SchwabOAuthManager("user123")
  val userId = "user123"
  val account = new SchwabAccount()

  // Example of handling Mono
  def handleTokenRefresh(): Unit = {
    // Refresh with Account
    oauthManager.refreshAccessTokenWithAccount(account).foreach { monoAccount =>
      monoAccount.subscribe(
        new JConsumer[SchwabAccount] {
          def accept(account: SchwabAccount): Unit = {
            println(s"Refreshed Account: ${account.getUserId}")
          }
        },
        new JConsumer[Throwable] {
          def accept(error: Throwable): Unit = {
            println(s"Error: ${error.getMessage}")
          }
        }
      )
    }

    // Refresh with UserID
    oauthManager.refreshAccessTokenWithUserId(userId).foreach { monoAccount =>
      monoAccount.subscribe(
        new JConsumer[SchwabAccount] {
          def accept(account: SchwabAccount): Unit = {
            println(s"Refreshed Account User ID: ${account.getUserId}")
          }
        },
        new JConsumer[Throwable] {
          def accept(error: Throwable): Unit = {
            println(s"Error refreshing account: ${error.getMessage}")
          }
        }
      )
    }
  }


  // Run the example
  handleTokenRefresh()
}
*/
