package exercises.errorhandling.either

import exercises.errorhandling.option.OptionExercises.{Email, User, UserId}
import exercises.errorhandling.either.EitherExercises1.UserEmailError._

import scala.util.control.NoStackTrace

object EitherExercises1 {

  // 1. Implement `getUserEmail` which looks up the email address of a user.
  // The email may be missing if:
  // * the user doesn't exist, or
  // * the user exists but doesn't have an email address
  // For example,
  // val users = Map(
  //   222 -> User(222, "john" , Admin, Some("j@x.com")),
  //   123 -> User(123, "elisa", Admin, Some("e@y.com")),
  //   444 -> User(444, "bob"  , Admin, None)
  // )
  // getUserEmail(123, users) == Right("e@y.com")
  // getUserEmail(111, users) == Left("User 111 is missing")
  // getUserEmail(444, users) == Left("User 444 has no email address")
  def getUserEmail(userId: UserId, users: Map[UserId, User]): Either[String, Email] =
//    users.get(userId).toRight(s"User ${userId.value} is missing").map(_.email).flatMap(_.toRight(s"User ${userId.value} has no email address"))
    for {
      user <- users.get(userId).toRight(s"User ${userId.value} is missing")
      email <- user.email.toRight(s"User ${userId.value} has no email address")
    } yield email

  // 2. Refactor `getUserEmail` so that it uses an `UserEmailError` instead of `String`
  // in the error channel.
  sealed abstract class UserEmailError(message: String) extends NoStackTrace {
    override def getMessage: String = message
  }
  object UserEmailError {
    case class UserNotFound(userId: UserId)  extends UserEmailError(s"User $userId is missing")
    case class EmailNotFound(userId: UserId) extends UserEmailError(s"User $userId has no email")
  }
  // In Scala 3,
  // enum UserEmailError {
  //   case UserNotFound, EmailNotFound
  // }

  // 3. Implement `errorMessage` which creates a human readable error message
  // from a `UserEmailError` object.
  // Note: Once implemented, move this method inside `UserEmailError`
  def errorMessage(error: UserEmailError): String =
    ???
}
