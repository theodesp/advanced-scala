object ch7 extends App {
  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  show(Nil)
  // res0: String = nil

  show(List(1, 2, 3))
  // res1: String = 3 then 2 then 1 then nil

  List(1, 2, 3).foldLeft(0)(_ + _)
  // res2: Int = 6

  List(1, 2, 3).foldRight(0)(_ + _)
  // res3: Int = 6

  List(1, 2, 3).foldLeft(0)(_ - _)
  // res4: Int = -6

  List(1, 2, 3).foldRight(0)(_ - _)
  // res5: Int = 2

  List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
  // res6: List[Int] = List(3, 2, 1)

  List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)
  // res7: List[Int] = List(1, 2, 3)

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }

  map(List(1, 2, 3))(_ * 2)
  // res9: List[Int] = List(2, 4, 6)

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }

  flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))
  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if(func(item)) item :: accum else accum
    }

  filter(List(1, 2, 3))(_ % 2 == 1)
  // res11: List[Int] = List(1, 3)

  import cats.Foldable
  import cats.instances.list._ // for Foldable

  val ints = List(1, 2, 3)

  Foldable[List].foldLeft(ints, 0)(_ + _)

  import cats.instances.option._ // for Foldable

  val maybeInt = Option(123)

  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
  // res3: Int = 1230

  import cats.instances.stream._ // for Foldable
  import cats.Eval
  import cats.Foldable

  def bigData = (1 to 100000).to(LazyList)

  val eval: Eval[Long] =
    Foldable[LazyList].
      foldRight(bigData, Eval.now(0L)) { (num, eval) =>
        eval.map(_ + num)
      }

  eval.value
  // res7: Long = 5000050000

  import cats.instances.vector._ // for Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.string._ // for Monoid

  Foldable[Option].nonEmpty(Option(42))
  // res10: Boolean = true

  Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)
  // res11: Option[Int] = Some(2)

  Foldable[List].combineAll(List(1, 2, 3))
  // res12: Int = 6

  Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  // res13: String = 123

  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))

  (Foldable[List] compose Foldable[Vector]).combineAll(ints)
  // res15: Int = 21

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val allUptimes: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  Await.result(allUptimes, 1.second)
  // res3: List[Int] = List(1020, 960, 840)
}