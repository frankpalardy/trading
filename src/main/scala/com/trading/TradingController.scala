package com.trading

import com.pangility.schwab.api.client.marketdata.SchwabMarketDataApiClient
import com.pangility.schwab.api.client.oauth2.{SchwabAccount, SchwabOauth2Controller}
import org.springframework.beans.factory.annotation.{Autowired, Value}
import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.autoconfigure.mongo.{MongoAutoConfiguration, MongoReactiveAutoConfiguration}
import org.springframework.context.ApplicationContext
import org.springframework.web.bind.annotation.{GetMapping, RequestParam, RestController}
import org.springframework.web.servlet.mvc.support.RedirectAttributes
import org.springframework.web.servlet.view.RedirectView

import java.util

@SpringBootApplication(exclude = Array(
  classOf[MongoAutoConfiguration],
  classOf[MongoReactiveAutoConfiguration]
))
@RestController
class TradingController {
  //The client will be injected here by the library, and can be used anywhere in the app.
  @Autowired
  private val schwabMarketDataApiClient: SchwabMarketDataApiClient = new SchwabMarketDataApiClient()
  //The manager will be injected here to have access to the methods.
  @Autowired
  private val schwabOAuthManager: SchwabOAuthManager = new SchwabOAuthManager("")

  @Autowired
  private val schwabOauth2Controller: SchwabOauth2Controller = new SchwabOauth2Controller()

  @GetMapping(Array("/"))
  def home(): String = {
    "Welcome to the application"
  }
}

object TradingController {

  def main(args: Array[String]): Unit = {
    val context: ApplicationContext = SpringApplication.run(classOf[TradingController], args: _*)
    // Get the SchwabOAuthManager bean from the context.
    val manager: SchwabOAuthManager = context.getBean(classOf[SchwabOAuthManager])
    // Initialize the SchwabMarketDataApiClient using the manager.
    manager.init()
    //No need to get the client since it's autowired in the controller.
  }
}