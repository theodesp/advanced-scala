object ch2 extends App {
  // A semigroup is just the combine part of a monoid.

  //trait Semigroup[A] {
  //  def combine(x: A, y: A): A
  //}
  //
  //trait Monoid[A] extends Semigroup[A] {
  //  def empty: A
  //}
  //
  //def associativeLaw[A](x: A, y: A, z: A)
  //                     (implicit m: Monoid[A]): Boolean = {
  //  m.combine(x, m.combine(y, z)) ==
  //    m.combine(m.combine(x, y), z)
  //}
  //
  //def identityLaw[A](x: A)
  //                  (implicit m: Monoid[A]): Boolean = {
  //  (m.combine(x, m.empty) == x) &&
  //    (m.combine(m.empty, x) == x)
  //}
  //
  //object Monoid {
  //  def apply[A](implicit monoid: Monoid[A]) =
  //    monoid
  //}
  //
  //
  //implicit val booleanAndMonoid: Monoid[Boolean] =
  //  new Monoid[Boolean] {
  //    def combine(a: Boolean, b: Boolean) = a && b
  //    def empty = true
  //  }
  //
  //implicit val booleanOrMonoid: Monoid[Boolean] =
  //  new Monoid[Boolean] {
  //    def combine(a: Boolean, b: Boolean) = a || b
  //    def empty = false
  //  }
  //
  //implicit val booleanEitherMonoid: Monoid[Boolean] =
  //  new Monoid[Boolean] {
  //    def combine(a: Boolean, b: Boolean) =
  //      (a && !b) || (!a && b)
  //
  //    def empty = false
  //  }
  //
  //implicit val booleanXnorMonoid: Monoid[Boolean] =
  //  new Monoid[Boolean] {
  //    def combine(a: Boolean, b: Boolean) =
  //      (!a || b) && (a || !b)
  //
  //    def empty = true
  //  }
  //
  //implicit def setUnionMonoid[A]: Monoid[Set[A]] =
  //  new Monoid[Set[A]] {
  //    def combine(a: Set[A], b: Set[A]) = a union b
  //    def empty = Set.empty[A]
  //  }
  //
  //val intSetMonoid = Monoid[Set[Int]]
  //val strSetMonoid = Monoid[Set[String]]
  //
  //intSetMonoid.combine(Set(1, 2), Set(2, 3))
  //
  //strSetMonoid.combine(Set("A", "B"), Set("B", "C"))
  //
  //implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
  //  new Semigroup[Set[A]] {
  //    def combine(a: Set[A], b: Set[A]) =
  //      a intersect b
  //  }
  //
  //implicit def symDiffMonoid[A]: Monoid[Set[A]] =
  //  new Monoid[Set[A]] {
  //    def combine(a: Set[A], b: Set[A]): Set[A] =
  //      (a diff b) union (b diff a)
  //    def empty: Set[A] = Set.empty
  //  }

  import cats.Monoid
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.semigroup._ // for |+|

  Monoid[String].combine("Hi ", "there")
  Monoid[String].empty
  Monoid[Int].combine(32, 10)
  val a = Option(22)
  val b = Option(20)
  Monoid[Option[Int]].combine(a, b)

  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty

  import cats.instances.int._ // for Monoid

  val intResult = 1 |+| 2 |+| Monoid[Int].empty

  import cats.Monoid
  import cats.instances.int._    // for Monoid
  import cats.syntax.semigroup._ // for |+|

  def add(items: List[Int]): Int =
    items.foldLeft(Monoid[Int].empty)(_ |+| _)

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoid: Monoid[Order] = new Monoid[Order] {
    def combine(o1: Order, o2: Order) =
      Order(
        o1.totalCost + o2.totalCost,
        o1.quantity + o2.quantity
      )

    def empty = Order(0, 0)
  }
}