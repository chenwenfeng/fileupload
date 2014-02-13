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
  
  def packs(email: String): List[String] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select packs from User u
          where u.email = {email};
        """
      ).on("email" -> email).apply().head match {
        case Row(Some(packs: List[String])) => packs
        case _ => Nil
      }
    }
  }

  def checkPackNameAvaiable(email: String, packName: String): Boolean = {
    packs(email).indexOf(packName) == -1
  }

  implicit def rowToStringArray: Column[Array[String]] = {
    Column.nonNull[Array[String]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case ss: Array[String] => Right(ss)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }


  def getPictures(email: String, pack: String): List[String] = {
    DB.withConnection { implicit c =>
      val results: Stream[Array[String]] = SQL(
        """
          select pictures from Pack p
          where p.email = {email} and p.pack = {pack};
        """
      ).on("email" -> email, "pack" -> pack)().map { row =>
        row[Array[String]]("pictures")
      }
      results.headOption match {
        case Some(x) => x.toList
        case None => Nil
      }
    }
  }

  def checkPicturesExist(email: String, packName: String): Boolean = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select count(*) from Pack p
          where p.email = {email} and p.pack = {pack};
        """
      ).on("email" -> email, "pack" -> packName).as(scalar[Long].single) == 1
    }
  }

  def updatePack(email: String, packName: String, pictures: List[String]) {
    if(checkPicturesExist(email, packName)) {
      val pictureList = (getPictures(email, packName) ++ pictures).toArray
      DB.withConnection { implicit c =>
        val r = SQL(
          """
            update Pack p set pictures = {pictureList}
            where p.email = {email} and p.pack = {packName};
          """
        ).on("pictureList" -> pictureList, "email" -> email, "packName" -> packName).executeUpdate()
        println(r)
      }
    } else {
      val pictureList = pictures.toArray
      DB.withConnection { implicit c =>
        val id: Option[Long] = SQL("insert into Pack(email, pack, pictures) values ({email}, {pack}, {pictures})")
          .on('email -> email, 'pack -> packName, 'pictures -> pictures).executeInsert()
        println(id)
      }
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
          where u.email = {email} and u.password = {password};
        """
      ).on("email" -> email, "password" -> password).as(scalar[Long].single)
      count == 1
    }
  }

  def checkAvaiable(email: String): Boolean = {
    DB.withConnection { implicit c =>
      val count = SQL(
        """
          select count(*) from User u  
          where u.email = {email};
        """
      ).on("email" -> email).as(scalar[Long].single)
      count == 0
    }
  } 
}