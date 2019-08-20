object ch8 extends App {
  import scala.concurrent.Future

  import cats.instances.future._ // for Applicative
  import cats.instances.list._   // for Traverse
  import cats.syntax.traverse._  // for traverse
  import cats.syntax.functor._  // for traverse
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.language.higherKinds
  import cats.Id
  import cats.Applicative

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int]
  }

  class UptimeService[F[_]: Applicative]
  (client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int])
    extends UptimeClient[Id] {
    def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  def testTotalUptime() = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()

  import cats.Monoid
  import cats.instances.int._    // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.semigroup._ // for |+|

  def foldMap[A, B : Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))

  trait Check[E, A] {
    def apply(value: A): Either[E, A]

    // other methods...
  }

  import cats.Semigroup
  import cats.instances.list._   // for Semigroup
  import cats.syntax.semigroup._ // for |+|

  val semigroup = Semigroup[List[String]]

  // Combination using methods on Semigroup
  semigroup.combine(List("Badness"), List("More badness"))
  // res2: List[String] = List(Badness, More badness)

  // Combination using Semigroup syntax
  List("Oh noes") |+| List("Fail happened")
  // res4: List[String] = List(Oh noes, Fail happened)

  import cats.Semigroup
  import cats.syntax.either._    // for asLeft and asRight
  import cats.syntax.semigroup._ // for |+|

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] =
      func(a)

    def and(that: CheckF[E, A])
           (implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1),  Left(e2))  => (e1 |+| e2).asLeft
          case (Left(e),   Right(a))  => e.asLeft
          case (Right(a),  Left(e))   => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
      }
  }

  import cats.instances.list._ // for Semigroup

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: CheckF[List[String], Int] =
    a and b

  check(5)
  // res8: Either[List[String],Int] = Left(List(Must be < -2))

  check(0)
  // res9: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._     // for mapN
  import cats.data.Validated._   // for Valid and Invalid


  sealed trait Check2[E, A] {
    def and(that: Check2[E, A]): Check2[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(e1),  Left(e2))  => (e1 |+| e2).asLeft
            case (Left(e),   Right(a))  => e.asLeft
            case (Right(a),  Left(e))   => e.asLeft
            case (Right(a1), Right(a2)) => a.asRight
          }
        case Or(left, right) =>
          left(a) match {
            case Valid(a)    => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](
                              left: Check2[E, A],
                              right: Check2[E, A]) extends Check2[E, A]

  final case class Pure[E, A](
                               func: A => Either[E, A]) extends Check2[E, A]

  final case class Or[E, A](
                             left: Check2[E, A],
                             right: Check2[E, A]) extends Check2[E, A]

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter): GCounter =
      GCounter(that.counters ++ this.counters.map {
        case (k, v) =>
          k -> (v max that.counters.getOrElse(k, 0))
      })

    def total: Int =
      counters.values.sum
  }

  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intInstance: BoundedSemiLattice[Int] =
      new BoundedSemiLattice[Int] {
        def combine(a1: Int, a2: Int): Int =
          a1 max a2

        val empty: Int =
          0
      }

    implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
      new BoundedSemiLattice[Set[A]]{
        def combine(a1: Set[A], a2: Set[A]): Set[A] =
          a1 union a2

        val empty: Set[A] =
          Set.empty[A]
      }
  }

  import cats.instances.list._   // for Monoid
  import cats.instances.map._    // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._  // for combineAll

  final case class GCounter[A](counters: Map[String,A]) {
    def increment(machine: String, amount: A)
                 (implicit m: CommutativeMonoid[A]): GCounter[A] = {
      val value = amount |+| counters.getOrElse(machine, m.empty)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter[A])
             (implicit b: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(this.counters |+| that.counters)

    def total(implicit m: CommutativeMonoid[A]): A =
      this.counters.values.toList.combineAll
  }

  import cats.instances.list._   // for Monoid
  import cats.instances.map._    // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._  // for combineAll

  implicit def mapInstance[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      def increment(map: Map[K, V])(key: K, value: V)
                   (implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val total = map.getOrElse(key, m.empty) |+| value
        map + (key -> total)
      }

      def merge(map1: Map[K, V], map2: Map[K, V])
               (implicit b: BoundedSemiLattice[V]): Map[K, V] =
        map1 |+| map2

      def total(map: Map[K, V])
               (implicit m: CommutativeMonoid[V]): V =
        map.values.toList.combineAll
    }

  trait KeyValueStore[F[_,_]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
        f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)

      override def getOrElse[K, V](f: Map[K, V])
                                  (k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }

  implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {
    def put(key: K, value: V)
           (implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)
                 (implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }

  implicit def gcounterInstance[F[_,_], K, V]
  (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)
                   (implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])
               (implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }
}