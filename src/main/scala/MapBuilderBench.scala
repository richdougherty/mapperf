package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.generic.{CanBuildFrom, MapFactory}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(4)
@Warmup(iterations = 12)
@State(Scope.Benchmark)
class MapBuilderBench {

  @Param(Array("old","new"))
  var factory: String = _
  @Param(Array("0","1","2","3","4","5","8","32","128","4096"))
  var size: Int = _

  var factoryObj: MapFactory[scala.collection.Map] = _
  var elems: Array[(String,String)] = _
  var elemMap: Map[String,String] = _
  type CBF = CanBuildFrom[Map[String,String], (String, String), Map[String, String]]
  implicit var cbf: CBF = _

  @Setup(Level.Trial)
  def trialInit(): Unit = {
    factory match {
      case "old" =>
        factoryObj = scala.collection.Map
        cbf = scala.collection.Map.canBuildFrom[String,String].asInstanceOf[CBF]
      case "new" =>
        factoryObj = scala.collection.Map2
        cbf = scala.collection.Map2.canBuildFrom[String,String].asInstanceOf[CBF]
    }
  }

  @Setup(Level.Iteration)
  def iterationInit(): Unit = {
    val time = System.currentTimeMillis()
    elems = (0 until size).map(i => (s"key$i-$time" -> s"val$i-$time")).toArray
    elemMap = elems.toMap
  }

  @Benchmark def mapFactoryBuildWithSizeHint(bh: Blackhole): Unit = {
    val builder = factoryObj.newBuilder[String,String]
    builder.sizeHint(size)
    var i = 0
    while (i < size) {
      builder +=(elems(i))
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark def mapFactoryBuildWithoutSizeHint(bh: Blackhole): Unit = {
    val builder = factoryObj.newBuilder[String,String]
    var i = 0
    while (i < size) {
      builder +=(elems(i))
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark def mapFactoryApplyArray(bh: Blackhole): Unit = {
    bh.consume(factoryObj.apply(elems: _*))
  }

  @Benchmark def mapMap(bh: Blackhole): Unit = {
    bh.consume(elemMap.map(identity)(cbf))
  }
}
