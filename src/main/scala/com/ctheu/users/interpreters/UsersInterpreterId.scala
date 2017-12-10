package com.ctheu.users.interpreters

import cats.{Id, ~>}
import com.ctheu.users.{User, Users}

/*
 * Not pure :-(
 */
object UsersInterpreterId extends (Users.UserCommand ~> Id) {
  import Users._

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
