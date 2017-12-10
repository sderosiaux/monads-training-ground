package com.ctheu.users.dsls

import cats.free.Free
import cats.free.Free.liftF
import com.ctheu.users.User
import com.ctheu.users.Users._

object Dsl {
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
