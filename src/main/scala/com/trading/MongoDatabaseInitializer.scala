package com.trading

import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Indexes
import org.springframework.context.annotation.Configuration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

@Configuration
object MongoConfig {
  private val uri = "${mongodb.uri}"
  private val databaseName = "${mongodb.database}"
  private val collectionName = "${mongodb.collection}"

  def getCollection: Future[MongoCollection[Document]] = {
    val promise = Promise[MongoCollection[Document]]()

    try {
      val client: MongoClient = MongoClient(uri)
      val database = client.getDatabase(databaseName)
      val collection = database.getCollection(collectionName)

      collection.createIndex(
        Indexes.compoundIndex(
          Indexes.ascending("symbol"),
          Indexes.ascending("date")
        )
      ).toFuture().map { _ =>
        promise.success(collection)
        collection
      }.recover {
        case e: Exception =>
          promise.failure(e)
          throw e
      }
    } catch {
      case e: Exception =>
        promise.failure(e)
        Future.failed(e)
    }

    promise.future
  }
}

object MongoDatabaseInitializer {
  def createTableAndLoadData(data: Seq[Asset]): Future[Unit] = {
    MongoConfig.getCollection.flatMap { collection =>
      insertData(collection, data)
    }.recover {
      case e: Exception =>
        println(s"Unexpected error: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  private def insertData(collection: MongoCollection[Document], data: Seq[Asset]): Future[Unit] = {
    if (data.isEmpty) {
      Future.successful(())
    } else {
      val documents = data.map { stockPrice =>
        Document(
          "symbol" -> stockPrice.symbol,
          "date" -> stockPrice.date,
          "closePrice" -> stockPrice.closePrice,
          "timestamp" -> stockPrice.timestamps,
          "lows" -> stockPrice.lows,
          "highs" -> stockPrice.highs,
          "close" -> stockPrice.prices,
          "volume" -> stockPrice.volumes
        )
      }

      collection.insertMany(documents)
        .toFuture()
        .map(_ => ())
        .recover {
          case e: Exception =>
            println(s"Error inserting documents: ${e.getMessage}")
            e.printStackTrace()
        }
    }
  }
}