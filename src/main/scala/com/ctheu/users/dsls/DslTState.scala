package com.ctheu.users.dsls

import cats.data.State
import cats.free.FreeT
import com.ctheu.users.User
import com.ctheu.users.Users._

object DslTState {
  type UserCommandTState[A] = FreeT[UserCommand, UserCommandState, A]

  // Intellij needs help with the types
  def create(id: String): UserCommandTState[User] = FreeT.liftF[UserCommand, UserCommandState, User](Create(id))
  def rename(id: String, newName: String) = FreeT.liftF[UserCommand, UserCommandState, Option[User]](Rename(id, newName))
  def find(id: String) = FreeT.liftF[UserCommand, UserCommandState, Option[User]](Find(id))
  def all() = FreeT.liftF[UserCommand, UserCommandState, List[User]](All())
  def cleanup() = FreeT.liftF[UserCommand, UserCommandState, Unit](Cleanup())
}
