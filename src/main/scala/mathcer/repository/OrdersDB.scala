package mathcer.repository

import mathcer.domain.Domain.{Client, Order}
import scala.collection.concurrent.TrieMap

/**
 * in-memory DB for Orders
 */
trait OrdersDB {
  //  a type alias for the service
  type OrdersService = Service

  trait Service {
    def add(order: Order): Unit
    def addAll(orders: List[Order]): Unit
    def findAll(): List[(Long, Order)]
  }
}

object OrdersDB extends OrdersDB {
  object OrdersDBImpl extends Service {
    private val orderDB: TrieMap[Long, Order] = TrieMap[Long, Order]()

    def add(order: Order): Unit = {
      val id = orderDB.keySet.size
      orderDB.put(id, order.copy(id = id))
    }

    def addAll(orders: List[Order]): Unit =
      orders.foreach(add)

    def findAll(): List[(Long, Order)] =
      orderDB.toList
  }

  // live for environment
  val ordersService: Service = OrdersDBImpl
}