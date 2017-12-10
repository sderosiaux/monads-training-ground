package com.ctheu.users.dsls

import cats.Id
import cats.free.FreeT
import com.ctheu.users.User
import com.ctheu.users.Users._

object DslT {
  type UserCommandT[M[_], A] = FreeT[UserCommand, M, A]

  def create(id: String): UserCommandT[Id, User] = FreeT.liftF(Create(id))
  def rename(id: String, newName: String) = FreeT.liftF(Rename(id, newName))
  def find(id: String) = FreeT.liftF(Find(id))
  def all() = FreeT.liftF(All())
  def cleanup() = FreeT.liftF(Cleanup())
}
