package mathcer.domain

import mathcer.domain.Domain.Currencies.Crrency
import mathcer.domain.Domain.OrderOperation.OrderOperType
import mathcer.domain.Domain.Securities.Security
import mathcer.exceptions.UndefinedCurrencyException
import mathcer.repository.ClientDB.ClientService
import mathcer.repository.OrdersDB.OrdersService
import mathcer.service.Matcher.MatcherService
import mathcer.util.FileUtil.FileUtils
import java.util.Date

/**
 * Domain objects
 *
 */
object Domain {

  private type Id = Long
  private type ClientName = String
  private type ClientBalanceCurrency = BigDecimal
  private type ClientBalanceAmountSecurityA = Int
  private type ClientBalanceAmountSecurityB = Int
  private type ClientBalanceAmountSecurityC = Int
  private type ClientBalanceAmountSecurityD = Int

  private type OrderOprType = OrderOperType
  private type OrderSecurityType = Security
  private type OrderPrice = BigDecimal

  case class Environment(fileUtils: FileUtils, orders: OrdersService, clients: ClientService, matcherService: MatcherService)

  /**
   * Валюта биржи
   * на данные момент используется только доллар, $
   */
  object Currencies extends Enumeration {
    type Crrency = Value

    val UsdCurrency = Value
  }

  /**
   * Ценные бумаги, участвующие в торгах
   *
   */
  object Securities extends Enumeration {
    type Security = Value

    val A, B, C, D = Value

    def parse(securityType: String): Security =
      securityType match {
        case "A" => Securities.A
        case "B" => Securities.B
        case "C" => Securities.C
        case "D" => Securities.D
      }
  }

  /**
   * Ценные бумаги, участвующие в торгах
   *
   */
  object OrderOperation extends Enumeration {
    type OrderOperType = Value

    val s, b = Value

    def parse(operationRawValue: String): OrderOperType =
      operationRawValue match {
        case "s" => OrderOperation.s
        case "b" => OrderOperation.b
      }
  }


  // Валюта
  case class Currency(value: Crrency)

  object Currency {
    def apply(currency: String): Either[UndefinedCurrencyException, Currency] =
      currency match {
        case "$" => Right(Currency(Currencies.UsdCurrency))
        case curr => Left(new UndefinedCurrencyException(curr))
      }
  }


  // Клиент
  case class Client(
                     id: Id = 1L,
                     name: ClientName,
                     balanceCurrency: ClientBalanceCurrency,
                     balanceSecurityAmountA: ClientBalanceAmountSecurityA,
                     balanceSecurityAmountB: ClientBalanceAmountSecurityB,
                     balanceSecurityAmountC: ClientBalanceAmountSecurityC,
                     balanceSecurityAmountD: ClientBalanceAmountSecurityD,
                   ) {
    require(name.nonEmpty, "Name must not be empty")
    require(balanceCurrency.longValue >= 0, "Balance could not be negative")
    require(balanceSecurityAmountA >= 0, "Balance A could not be negative")
    require(balanceSecurityAmountB >= 0, "Balance B could not be negative")
    require(balanceSecurityAmountC >= 0, "Balance C could not be negative")
    require(balanceSecurityAmountD >= 0, "Balance D could not be negative")

    override def toString: String =
      s"${name}  ${balanceCurrency.longValue}    ${balanceSecurityAmountA.longValue()}   ${balanceSecurityAmountB.longValue()}   ${balanceSecurityAmountC.longValue()}  ${balanceSecurityAmountD.longValue()}"
  }

  // Заказ
  case class Order(
                    id: Id,
                    clientName: ClientName,
                    operationType: OrderOperType,
                    securityName: Security,
                    securityUnderOperationAmount: Int,
                    price: OrderPrice,
                    createdAt: Date = new Date()
                  )

  object Order {
    def apply(
               id: Id,
               clientName: ClientName,
               operationType: OrderOprType,
               securityName: OrderSecurityType,
               securityUnderOperationAmount: Int,
               price: OrderPrice
             ): Order = {
      require(
        id >= 0 &&
          clientName.nonEmpty &&
          securityUnderOperationAmount.longValue() >= 0 &&
          price.longValue >= 0
      )
      Order(
        id,
        clientName,
        operationType,
        securityName,
        securityUnderOperationAmount,
        price,
        new Date()
      )
    }

  }

}
