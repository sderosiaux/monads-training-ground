package com.ctheu.users.interpreters

import cats.~>
import com.ctheu.users.{User, Users}

/*
 * Not pure :-(
 */
object UsersInterpreterOption extends (Users.UserCommand ~> Option) {
  import Users._

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
