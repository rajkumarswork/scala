package rajkumars.info.util

import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util._
import scala.concurrent.Future
import scala.concurrent.duration._

// A General-purpose Scheduler
//    It executes, each period, all the functions f() added through add(f()) where
//    f() needs to have the signature f():Future[Boolean] to ensure it is async and doesn't block
//    By default functions are run, until stop() is called, with no initial-delay
//    An optional count and initial delay can be specified
// Note: Beware of closures

class Scheduler(
    period: FiniteDuration,
    maxCount: Int = 0,
    delay: FiniteDuration = 0 millis
) {

  // - Api --------
  def add(f: () => Future[Boolean]) = fs = fs ++ Seq(f)
  def start(): Cancellable = {
    val countString = if (maxCount > 0) s" $maxCount times" else ""
    println(
      s"Scheduler :: will run ${fs.size} function(s) every $period $countString"
    )
    c = Scheduler.scheduler.schedule(initialDelay = delay, period, runnable)(
      Scheduler.executor
    )
    c
  }
  def stop(c: Cancellable) = c.cancel()
  def currentCount(): Int = count

  // - Privates --------
  var fs: Seq[() => Future[Boolean]] = Seq[() => Future[Boolean]]()
  val runnable = new Runnable {
    def run() = {
      fs.foreach(f => f())
      count = count + 1
      if (maxCount > 0 && count >= maxCount) c.cancel()
    }
  }
  var c: Cancellable = Scheduler.scheduler.scheduleOnce(1 hour)(() => {})
  var count = 0
}

// -----------------
object Scheduler {

  val as = ActorSystem()
  val scheduler = as.scheduler
  val executor = as.dispatcher

  def kill(): Unit = as.terminate()
}
