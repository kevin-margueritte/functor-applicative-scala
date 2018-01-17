import kmargueritte.option.{Maybe, Tadam}
import org.scalacheck.Prop
import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {

  "Maybe" should "verified composition and identity" in {

    import kmargueritte.option.applicative._
    import kmargueritte.syntax.ApplicativeImpl

    Prop.forAll { (a: Int, b: Int, c: Int) =>

      val x: Maybe[Int] = Tadam(a)
      val f: (Int => Int) = (y: Int) => y + b
      val g: (Int => Int) = (y: Int) => y + c

      // Composition
      x.map(f(_)).map(g(_)) == x.map(f.andThen(g))

      // Identity
      x.map(x => x) == x

    }

  }

}
