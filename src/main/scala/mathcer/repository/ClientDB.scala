package mathcer.repository

import mathcer.domain.Domain.Client
import scala.collection.concurrent.TrieMap

/**
 * Client in-memory DB
 */
trait ClientDB {
  //  a type alias for the service
  type ClientService = Service

  trait Service {
    def add(client: Client): Option[Client]
    def findByName(name: String): Option[Client]
    def findAll(): List[Client]
    def updateClient(client: Client): Option[Client]
  }
}

object ClientDB extends ClientDB {
  object ClientDBImpl extends Service {
    private val clientDB: TrieMap[Long, Client] = TrieMap[Long, Client]()

    def add(client: Client): Option[Client] = {
      val id = clientDB.keySet.size
      clientDB.put(id, client.copy(id = id))
    }

    def findByName(name: String): Option[Client] =
      clientDB.find(_._2.name == name).map(_._2)

    def findAll(): List[Client] =
      clientDB.values.toList

    def updateClient(client: Client): Option[Client] = {
      clientDB.replace(client.id, client)
    }
  }

  // live for environment
  val clientService: Service = ClientDBImpl
}