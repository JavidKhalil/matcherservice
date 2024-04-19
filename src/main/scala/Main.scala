import mathcer.Effects
import mathcer.domain.Domain.Environment
import mathcer.repository.ClientDB.clientService
import mathcer.repository.OrdersDB.ordersService
import mathcer.service.Matcher.matcherService
import mathcer.util.FileUtil.fileService

object Main {
  def main(args: Array[String]): Unit = {
    Effects.run("src/main/resources")(Environment(fileService, ordersService, clientService, matcherService))
  }
}