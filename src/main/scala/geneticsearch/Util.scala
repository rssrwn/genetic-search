package geneticsearch

import scala.util.{Failure, Success, Try}


object Util {

    def extractSuccess[T](tryObj: Try[T]): T = {
        tryObj match {
            case Success(value) => value
            case Failure(exception) =>
                System.err.println(exception)
                sys.exit(1)
        }
    }

}
