import kmargueritte.option.{Maybe, Tadam}
import org.scalatest._
import org.scalacheck._

class FunctorSpec extends FlatSpec with Matchers {

 "Maybe" should "verified composition and identity" in {

    import kmargueritte.option.functor._
    import kmargueritte.syntax.FunctorImpl

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

  "Maybe" should "a map2 implementation" in {

    import kmargueritte.option.applicative._
    import kmargueritte.syntax.Map2

    Prop.forAll { (a: Int, b: Int) =>

      val fa: Maybe[Int] = Tadam(a)
      val fb: Maybe[Int] = Tadam(b)

      fa.map2(fb)((x,y) => x + y) == Tadam(a + b)

    }

  }

}
