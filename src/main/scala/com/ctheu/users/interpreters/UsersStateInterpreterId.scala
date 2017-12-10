package com.ctheu.users.interpreters

import cats.data.State
import cats.~>
import com.ctheu.users.{User, Users}

/*
 * Pure :-)
 */
object UsersStateInterpreterId extends (Users.UserCommand ~> Users.UserCommandState) {
  import Users._

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
