package rajkumars.info.util

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.FlatSpec

class SchedulerTest extends FlatSpec {

  "Scheduler" should "schedule a job to run x times correctly" in {
    var counter = 0
    val s = new Scheduler(5 seconds, 3)
    s.add(() => Future { counter = counter + 1; true })
    s.start()
    Thread.sleep(20000)
    assert(counter === 3)
  }
}
