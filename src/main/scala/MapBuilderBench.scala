package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.generic.MapFactory

@BenchmarkMode(Array(Mode.AverageTime))
// @Fork(2)
@Fork(1)
@Threads(1)
// @Warmup(iterations = 10)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
//@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class MapBuilderBench {

  @Param(Array("old","new"))
  var factory: String = _
  @Param(Array("0","1","2","3","4","5","8","32","128","512","2048"))
  var size: Int = _

  var factoryObj: MapFactory[scala.collection.Map] = _
  var elems: Array[(String,String)] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    factoryObj = factory match {
      case "old" => scala.collection.Map
      case "new" => scala.collection.Map2
    }
    elems = (0 until size).map(i => (s"key$i" -> s"val$i")).toArray
  }

  @Benchmark def mapFactoryBuildWithoutSizeHint(bh: Blackhole): Unit = {
    val builder = factoryObj.newBuilder[String,String]
    var i = 0
    val len = elems.length
    while (i < len) {
      builder +=(elems(i))
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark def mapFactoryApplyArray(bh: Blackhole): Unit = {
    bh.consume(factoryObj.apply(elems: _*))
  }
}
