package models

import play.api.Play.current
import play.api.mvc._
import anorm._ 
import anorm.SqlParser._
import play.api.db._

case class User(email: String, password: String)

object User {
  

  def all(): List[User] = {
    DB.withConnection { implicit c =>
      val users = SQL("select * from User").apply().map(row => 
          User(row[String]("email"), "")
        ).toList
      users
    }
  }
  
  def create(email: String, password: String) {
    DB.withConnection { implicit c =>
      val id: Option[Long] = SQL("insert into User(email, password) values ({email}, {password})")
        .on('email -> email, 'password -> password).executeInsert()    
    } 
  }

  def check(email: String, password: String): Boolean = {
    DB.withConnection { implicit c =>
      val count = SQL(
        """
          select count(*) from User u  
          where u.email = {email} and u.password == {password};
        """
      ).on("email" -> email, "password" -> password).as(scalar[Long].single)
      count == 1
    }
  } 
}