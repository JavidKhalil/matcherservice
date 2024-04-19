package mathcer

import mathcer.domain.Domain.Environment

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Effects {

  /**
   * Main point
   *
   * First we extract the list of customer account from the source file and store in-memory DB
   * After we do same thing for orders
   * and after start processing each order step by step
   * and write results to "result.txt"
   *
   * @param initialDataPath path to the initial files
   * @param env             Environment with required dependencies
   */
  def run(pathToSource: String)(implicit environment: Environment): Unit = {
    environment.matcherService.initClients(pathToSource).map { _ =>
      environment.matcherService.initOrders(pathToSource)
    }.onComplete {
      case Failure(exception) => println(s"calculation failed, please check error ${exception.getMessage}")
      case Success(_) => {
        val content = environment.matcherService.applyOrders().value match {
          case Some(value) => value match {
            case Failure(err) => err.getMessage
            case Success(listResults) => listResults.map {
              case Left(matchErr) => matchErr.getMessage
              case Right(maybeClient) => maybeClient match {
                case Some(clnt) => clnt.toString
                case None => "client account unreachable"
              }
            }.mkString("\n")
          }
          case None => "unexpected error, check again later"
        }
        environment.fileUtils.writeFile("src/main/resources/result.txt", content)
        println(s"calculation done\nplease find the result in \"result.txt\" file")
      }
    }
  }
}