package geneticsearch.algorithm

import java.util.concurrent.Executors

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


object Executor {

    private val execContext = ExecutionContext.fromExecutor(
            Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2))

    /**
      * Submit a job to be run on by an executor service
      *
      * @param code Code to be run
      * @tparam T Type of object returned by code
      * @return Future object, holding object if type T
      */
    def submit[T](code: => T): Future[T] = {
        Future {
            code
        }(execContext)
    }

    /**
      * Await a Future up to a maximum amount of time
      *
      * @param future Future object to await
      * @param duration Maximum wait time in millis
      * @tparam T Type of result held by future
      * @return Object returned from future
      */
    def await[T](future: Future[T], duration: Int): T = {
        Await.result(future, Duration(duration, "millis"))
    }

    /**
      * Await a Future forever
      *
      * @param future Future object to await
      * @tparam T Type of result held by future
      * @return Object returned from future
      */
    def await[T](future: Future[T]): T = {
        Await.result(future, Duration.Inf)
    }

}
