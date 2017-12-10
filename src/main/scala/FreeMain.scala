/*

https://blog.scalac.io/2016/06/02/overview-of-free-monad-in-cats.html

 */

import Users._
import cats._
import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import cats.implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class User(id: String)

object Users {
  type FreeUserCommand[A] = Free[UserCommand, A]
  type UserCommandState[A] = State[List[User], A]

  sealed trait UserCommand[A]
  final case class Create(id: String) extends UserCommand[User]
  final case class Find(id: String) extends UserCommand[Option[User]]
  final case class Rename(id: String, newName: String) extends UserCommand[Option[User]]
  final case class All() extends UserCommand[List[User]]
  final case class Cleanup() extends UserCommand[Unit]

  object dsl {
    def create(id: String) = liftF(Create(id))
    def rename(id: String, newName: String): Free[UserCommand, Option[User]] = liftF(Rename(id, newName))
    def find(id: String) = liftF(Find(id))
    def all() = liftF(All())
    def cleanup() = liftF(Cleanup())
    // we can't create other shortcuts
    def create(ids: List[String]): List[Free[UserCommand, User]] = ids.map(create)
    // and "computed" functions
    def addTitle(id: String, title: String): Free[UserCommand, Option[User]] = for {
      u <- find(id)
      r <- u.map(u => s"$title ${u.id}").map(rename(id, _)).getOrElse(Free.pure[UserCommand, Option[User]](None))
    } yield r
  }
}

/*
 * Not pure :-(
 */
object UsersInterpreterId extends (UserCommand ~> Id) {
  val users = collection.mutable.Map[String, User]()
  override def apply[A](fa: UserCommand[A]): Id[A] = fa match {
    // Intellij IDEA syntax highlighting complains and need .asInstanceOf[A] but compiles properly without
    case Create(id) => val u = User(id); users += id -> u; u
    case Find(id) => users.get(id)
    case All() => users.values.toList
    case Cleanup() => users.clear()
    case Rename(id, newName) =>
      users.get(id).map(_.copy(id = newName)) match {
        case Some(value) => users -= id; users += newName -> value; Some(value)
        case None => None
      }

  }
}

/*
 * Pure :-)
 */
object UsersStateInterpreterId extends (UserCommand ~> UserCommandState) {
  override def apply[A](fa: UserCommand[A]): UserCommandState[A] = fa match {
    case Create(id) => val newUser = User(id); State.modify[List[User]](_ :+ newUser).map(_ => newUser)
    case Find(id) => State.inspect[List[User], Option[User]](_.find(_.id == id))
    case Rename(id, newName) =>
      // TODO(sd): sure we can do better here!
      (for {
        u <- State.inspect[List[User], Option[User]](_.find(_.id == id))
        renamed = u.map(_.copy(id = newName))
        r <- State.modify[List[User]](x => u match {
          case Some(value) =>
            val i = x.indexOf(value)
            val renamedUser = renamed.get
            x.updated(i, renamedUser)
          case None => x
        }).map(_ => renamed)
      } yield r).map(_.asInstanceOf[A])

    case All() => State.get[List[User]]
    case Cleanup() => State.set[List[User]](List())
  }
}

/*
 * Not pure :-(
 */
object UsersInterpreterFuture extends (UserCommand ~> Future) {
  val users = collection.mutable.Map[String, User]()
  override def apply[A](fa: UserCommand[A]): Future[A] = fa match {
    // Intellij IDEA syntax highlighting complains and need .asInstanceOf[Future[A]] but compiles properly without
    case Create(id) => val u = User(id); users += id -> u; Future.successful(u)
    case Find(id) => Future.successful(users.get(id))
    case All() => Future.successful(users.values.toList)
    case Cleanup() => users.clear(); Future.unit
    case Rename(id, newName) =>
      users.get(id).map(_.copy(id = newName)) match {
        case Some(value) => users -= id; users += newName -> value;  Future.successful(Some(value))
        case None => Future.successful(None)
      }
  }
}

/*
 * Not pure :-(
 */
object UsersInterpreterOption extends (UserCommand ~> Option) {
  val users = collection.mutable.Map[String, User]()
  override def apply[A](fa: UserCommand[A]) = fa match {
    // Intellij IDEA syntax highlighting complains and need .asInstanceOf[Option[A]] but compiles properly without
    case Create(id) => val u = User(id); users += id -> u; Some(u)
    case Find(id) => Some(users.get(id))
    case All() => users.values.toList match {
      case Nil => None
      case x => Some(x)
    }
    case Cleanup() => users.clear(); None
    case Rename(id, newName) =>
      users.get(id).map(_.copy(id = newName)) match {
        case Some(value) => users -= id; users += newName -> value;  Some(Some(value))
        case None => None
      }
  }
}


object FreeMain extends App {
  import Users.dsl._

  def finderProgram(who: String) = for {
    _ <- create("sd")
    _ <- create("po")
    _ <- rename("po", "poooo")
    _ <- addTitle("poooo", "M.")
    u <- find(who)
    l <- all()
    _ <- cleanup()
  } yield (u, l)

  {
    // Id interpreter
    val (sd, all) = finderProgram("sd").foldMap(UsersInterpreterId)
    println(sd)
    println(all)
    // Some(User(sd))
    // List(User(sd), User(M. poooo))
  }

  println()

  {
    // Future interpreter
    import scala.concurrent.ExecutionContext.Implicits._
    val (sd, all) = Await.result(finderProgram("sd").foldMap(UsersInterpreterFuture), Duration.Inf)
    println(sd)
    println(all)
    // Some(User(sd))
    // List(User(sd), User(M. poooo))
  }

  println()

  {
    val emptyAllProgram = all()
    import scala.concurrent.ExecutionContext.Implicits._

    // Option interpreter
    println(" id=" + emptyAllProgram.foldMap(UsersInterpreterId))
    println("fut=" + Await.result(emptyAllProgram.foldMap(UsersInterpreterFuture), Duration.Inf))
    println("opt=" + emptyAllProgram.foldMap(UsersInterpreterOption))

    //  id=List()
    // fut=List()
    // opt=None
  }

  println()

  {
    // UsersStateInterpreterId only (state is provided, not internal)
    println(create("sd").foldMap(UsersStateInterpreterId).run(List(User("jo"))).value)
    println(all().foldMap(UsersStateInterpreterId).run(List(User("jo"))).value)
    println(rename("sd", "toto").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)
    println(addTitle("sd", "M.").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)
    println(addTitle("sad", "M.").foldMap(UsersStateInterpreterId).run(List(User("jo"), User("sd"))).value)

    // (List(User(jo), User(sd)),User(sd))
    // (List(User(jo)),List(User(jo)))
    // (List(User(jo), User(toto)),Some(User(toto)))
    // (List(User(jo), User(M. sd)),Some(User(M. sd)))
    // (List(User(jo), User(sd)),None)
  }
}