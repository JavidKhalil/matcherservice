package mathcer.exceptions

/**
 * Matcher exceptions
 *
 * @param exception
 */
class MatcherExceptions(exception: String) extends Exception {
  override def getMessage: String = exception
}

case class ClientNotFoundException(msg: String) extends MatcherExceptions(msg) {
  override def getMessage: String = s"client not found ${msg}"
}
case class UndefinedCurrencyException(msg: String) extends MatcherExceptions(msg) {
  override def getMessage: String = s"undefined currency ${msg}"
}
case class NotEnoughMoneyException(msg: String) extends MatcherExceptions(msg) {
  override def getMessage: String = s"not enough money in account ${msg}"
}
case class NotEnoughSecuritiesException(msg: String, accountName: String) extends MatcherExceptions(msg) {
  override def getMessage: String = s"not enough security ${msg} in account ${accountName}"
}