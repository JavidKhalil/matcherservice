package mathcer.service

import mathcer.domain.Domain.{Client, Environment, Order, OrderOperation, Securities}
import mathcer.exceptions.{ClientNotFoundException, MatcherExceptions, NotEnoughMoneyException, NotEnoughSecuritiesException}
import mathcer.domain.Domain.Securities._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The service object
 *
 */

// Define a trait for Matcher functionality
trait Matcher {
  // Define a type alias for the service
  type MatcherService = Service

  // Define the service trait
  trait Service {
    def initClients(initialDataPath: String)(implicit environment: Environment): Future[Unit]

    def initOrders(initialDataPath: String)(implicit environment: Environment): Future[Unit]

    def applyOrders()(implicit environment: Environment): Future[List[Either[MatcherExceptions, Option[Client]]]]

    def operateSecurityOnBuy(cl: Client, dealAmount: BigDecimal, securityAmount: Int, security: Security, modifyClientAccount: (Client, BigDecimal, Int) => Right[Nothing, Option[Client]])(implicit environment: Environment): Either[MatcherExceptions, Option[Client]]

    def operateSecuritySale(cl: Client, operationAmount: Int, dealAmount: Long, security: Securities.Security)(implicit environment: Environment): Either[MatcherExceptions, Option[Client]]
  }
}

// Define the companion object for Matcher
object Matcher extends Matcher {
  // Implement the service trait
  object MatcherImpl extends Service {

    private val initialId = 1

    def initClients(initialDataPath: String)(implicit environment: Environment): Future[Unit] = Future.fromTry {
      Try {
        val lines = environment.fileUtils.readLines(initialDataPath + "/clients.txt")

        val res = lines.map(clientArr => {
          Client(
            initialId.toLong,
            String.valueOf(clientArr(0)),
            BigDecimal(String.valueOf(clientArr(1)).toInt),
            String.valueOf(clientArr(2)).toInt,
            String.valueOf(clientArr(3)).toInt,
            String.valueOf(clientArr(4)).toInt,
            String.valueOf(clientArr(5)).toInt
          )
        })
        res.foreach(environment.clients.add)
      }
    }

    def initOrders(initialDataPath: String)(implicit environment: Environment): Future[Unit] = Future.fromTry {
      Try {
        environment.fileUtils.readLines(initialDataPath + "/orders.txt").map(orderArr =>
          Order(initialId, orderArr(0), OrderOperation.parse(orderArr(1)), Securities.parse(orderArr(2)), orderArr(3).toInt, BigDecimal(orderArr(4)))
        ).foreach(environment.orders.add)
      }
    }


    def applyOrders()(implicit environment: Environment): Future[List[Either[MatcherExceptions, Option[Client]]]] = Future.fromTry {
      Try {
        val orders = environment.orders.findAll().map(_._2).sortBy(_.createdAt)

        orders.map { order => {
          val maybeClient = environment.clients.findByName(order.clientName)
          val wantToDeal = order.securityName
          val operationAmount = order.securityUnderOperationAmount
          val dealAmount = operationAmount.longValue() * order.price.bigDecimal.longValue()

          order.operationType match {
            case OrderOperation.s =>
              maybeClient match {
                case Some(cl) =>
                  operateSecuritySale(cl, operationAmount, dealAmount, wantToDeal)
                case None => Left(ClientNotFoundException(order.clientName))
              }
            case OrderOperation.b =>
              maybeClient match {
                case Some(cl) =>
                  operateSecurityOnBuy(cl, dealAmount, operationAmount, wantToDeal, (cl: Client, dealAmount: BigDecimal, securityAmount: Int) => Right(environment.clients.updateClient(cl.copy(
                    balanceCurrency = cl.balanceCurrency - dealAmount,
                    balanceSecurityAmountA = cl.balanceSecurityAmountA + securityAmount
                  ))))
                case None =>
                  Left(ClientNotFoundException(order.clientName))
              }
          }
        }
        }
      }
    }

    def operateSecurityOnBuy(cl: Client, dealAmount: BigDecimal, securityAmount: Int, security: Security, modifyClientAccount: (Client, BigDecimal, Int) => Right[Nothing, Option[Client]])(implicit environment: Environment): Either[MatcherExceptions, Option[Client]] = {
      if (cl.balanceCurrency.longValue >= dealAmount) {
        security match {
          case A => modifyClientAccount(cl, dealAmount, securityAmount)
          case B => modifyClientAccount(cl, dealAmount, securityAmount)
          case C => modifyClientAccount(cl, dealAmount, securityAmount)
          case D => modifyClientAccount(cl, dealAmount, securityAmount)
        }
      } else {
        Left(NotEnoughMoneyException(cl.balanceCurrency.longValue.toString))
      }
    }

    def operateSecuritySale(cl: Client, operationAmount: Int, dealAmount: Long, security: Security)(implicit environment: Environment): Either[MatcherExceptions, Option[Client]] = {
      if (operationAmount <= cl.balanceSecurityAmountA) {
        val updatedClient = cl.copy(
          balanceCurrency = cl.balanceCurrency + dealAmount,
          balanceSecurityAmountA = {
            security match {
              case A => cl.balanceSecurityAmountA - operationAmount
              case B => cl.balanceSecurityAmountB - operationAmount
              case C => cl.balanceSecurityAmountC - operationAmount
              case D => cl.balanceSecurityAmountD - operationAmount
            }
          }
        )
        Right(environment.clients.updateClient(updatedClient))
      } else {
        Left(NotEnoughSecuritiesException(security.toString, cl.name))
      }
    }
  }

  // Export the service as a val
  val matcherService: Service = MatcherImpl
}