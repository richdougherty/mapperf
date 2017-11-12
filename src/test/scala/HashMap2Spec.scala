package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HashMap2Spec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks  {

  "check allocation and equality work" in {
    forAll { elems: Seq[(Int, Int)] =>
      val m1 = Map(elems: _*)
      val m2 = Map2(elems: _*)
      assert(m1 == m2)
    }
  }

  "check iterator works" in {
    forAll { elems: Seq[(Int, Int)] =>
      val s1 = Map(elems: _*).iterator.toSeq
      val s2 = Map2(elems: _*).iterator.toSeq
      assert(s1 == s2)
    }
  }

  "check (''->'', ''->'x')" in {
    val elems = Vector("" -> "", "" -> "x")
    val m1 = Map(elems: _*)
    val m2 = Map2(elems: _*)
    assert(m1 == m2)
  }

  "check ((-1,0), (3,0), (-3,0), (1,0), (0,0))" in {
    val elems = Vector((-1,0), (3,0), (-3,0), (1,0), (0,0))
    val m1 = Map(elems: _*)
    val m2 = Map2(elems: _*)
    assert(m1 == m2)
  }

  "check ((0,0), (1,0), (-1,0), (-11085,0), (-32,0))" in {
    val elems = Vector((0,0), (1,0), (-1,0), (-11085,0), (-32,0))
    val m1 = Map(elems: _*)
    val m2 = Map2(elems: _*)
    assert(m1 == m2)
  }

  "check ((0,0), (-1,0), (1,0), (-2,0), (2,0), (0,-1))" in {
    val elems = Vector((0,0), (-1,0), (1,0), (-2,0), (2,0), (0,-1))
    val m1 = Map(elems: _*)
    val m2 = Map2(elems: _*)
    assert(m1 == m2)
  }

  "check size prediction works" in {
    import HashMap2.{predictCount => p}

    // Predictions for a size 0 trie

    assert(p(0,0) == 0)
    assert(p(0,1) == 1)
    assert(p(0,2) == 1)
    assert(p(0,3) == 2)
    assert(p(0,4) == 3)

    assert(p(0,8) == 5)
    assert(p(0,9) == 6)
    assert(p(0,10) == 6)

    assert(p(0,164) == 30)
    assert(p(0,165) == 31)
    assert(p(0,166) == 31)

    assert(p(0,252) == 31)
    assert(p(0,253) == 32)
    assert(p(0,254) == 32)

    // Predictions for a size 16 trie

    assert(p(16,0) == 16)
    assert(p(16,1) == 16)

    assert(p(16,6) == 16)
    assert(p(16,7) == 17)
    assert(p(16,8) == 17)
    assert(p(16,9) == 17)
    assert(p(16,10) == 17)
    assert(p(16,11) == 18)
    assert(p(16,12) == 18)

    assert(p(16,45) == 24)
    assert(p(16,46) == 25)
    assert(p(16,47) == 25)

    assert(p(16,232) == 31)
    assert(p(16,233) == 32)
    assert(p(16,234) == 32)

    // Predictions for a size 31 trie

    assert(p(31,0) == 31)
    assert(p(31,144) == 31)
    assert(p(31,145) == 32)
    assert(p(31,146) == 32)
  }
}
