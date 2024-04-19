import mathcer.domain.Domain.{Client, Environment, Securities}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import mathcer.service.Matcher._
import mathcer.exceptions._
import mathcer.repository.ClientDB.clientService
import mathcer.repository.OrdersDB.ordersService
import mathcer.util.FileUtil.fileService
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class MatcherSpec extends AnyWordSpec with Matchers with ScalaFutures with BeforeAndAfterAll {
  implicit val ec: ExecutionContext = ExecutionContext.global

  // to do rewrite with test live -s
  implicit val environment: Environment = (Environment(fileService, ordersService, clientService, matcherService))
  val initialDataPath = "src/test/resources"

  override protected def beforeAll(): Unit = {
    environment.matcherService.initClients(initialDataPath).map(_ =>
      environment.matcherService.initOrders(initialDataPath).map(_ => ()
      )).onComplete {
      case Failure(exception) =>
        println(s"Test context staring error ${exception.getMessage}")
      case Success(_) =>
        println(s"Test context stared successfully")
    }
  }

  override protected def afterAll(): Unit = {
    environment.matcherService.initClients(initialDataPath).map(_ =>
      environment.matcherService.initOrders(initialDataPath).map(_ =>
        ()
      )).onComplete {
      case Failure(exception) =>
        println(s"Test context shutdown error ${exception.getMessage}")
      case Success(_) =>
        println(s"Test context closed successfully")
    }
  }


  "Matcher service" should {

    "initialize clients successfully" in {

      // Ensure that initClients returns a successful Future[Unit]
      val initClientsResult: Future[Unit] = environment.matcherService.initClients(initialDataPath)
      whenReady(initClientsResult, Timeout(5.seconds)) { _ =>
        initClientsResult map { result => assert(result == ()) }
      }
    }

    "initialize orders successfully" in {

      // Ensure that initOrders returns a successful Future[Unit]
      val initOrdersResult: Future[Unit] = environment.matcherService.initOrders(initialDataPath)
      whenReady(initOrdersResult, Timeout(5.seconds)) { _ =>
        initOrdersResult map { result => assert(result == ()) }
      }
    }

    "apply orders successfully" in {

      // Ensure that applyOrders returns a successful Future[List[Either[MatcherExceptions, Option[Client]]]]
      val applyOrdersResult: Future[List[Either[MatcherExceptions, Option[Client]]]] = environment.matcherService.applyOrders()

      whenReady(applyOrdersResult, Timeout(5.seconds)) { _ =>
        applyOrdersResult map { result =>
          assert(
            result.size == 2
          )
        }
      }
    }

    "operate security on buy successfully" in {
      val client = Client(1L, "abc", BigDecimal.valueOf(100), 10, 20, 30, 40)
      val dealAmount = BigDecimal(100)
      val securityAmount = 10
      val security = Securities.A

      val result: Either[MatcherExceptions, Option[Client]] =
        environment.matcherService.operateSecurityOnBuy(client, dealAmount, securityAmount, security, (client, dealAmount, securityAmount) => Right(Some(client)))

      result match {
        case Left(value) => {
          assert {
            value == null
          }
        }
        case Right(cl) => {
          val result = cl.get
          assert {
            result.name == "abc" &&
              result.balanceCurrency == BigDecimal.valueOf(100) &&
              result.balanceSecurityAmountA == 10 &&
              result.balanceSecurityAmountB == 20 &&
              result.balanceSecurityAmountC == 30 &&
              result.balanceSecurityAmountD == 40
          }
        }
      }

    }

    "operate security sale successfully" in {
      val client = Client(1L, "abc", BigDecimal.valueOf(100), 15, 20, 30, 40)
      val operationAmount = 10
      val dealAmount = 100L
      val security = Securities.A

      // Ensure that operateSecuritySale returns a successful Either[MatcherExceptions, Option[Client]]
      val result: Either[MatcherExceptions, Option[Client]] = environment.matcherService.operateSecuritySale(client, operationAmount, dealAmount, security)
      result match {
        case Left(value) => {
          assert {
            value == null
          }
        }
        case Right(cl) => {
          val result = cl.get
          assert {
            result.name == "C2" &&
              result.balanceCurrency == BigDecimal.valueOf(2000) &&
              result.balanceSecurityAmountA == 3 &&
              result.balanceSecurityAmountB == 35 &&
              result.balanceSecurityAmountC == 40 &&
              result.balanceSecurityAmountD == 10
          }
        }
      }
    }
  }
}
