package com.ctheu.users.interpreters

import cats.~>
import com.ctheu.users.{User, Users}

import scala.concurrent.Future

/*
 * Not pure :-(
 */
object UsersInterpreterFuture extends (Users.UserCommand ~> Future) {
  import Users._

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
