package models

import play.api.Play.current
import play.api.mvc._
import anorm._ 
import anorm.SqlParser._
import play.api.db._

case class User(email: String, password: String)
case class Pack(packName: String, status: Int = -1, iapid: String = "")

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
        case Row(Some(packs: String)) => packs.split(",").toList
        case _ => Nil
      }
    }
  }

  def getAllPacks: List[Pack] = {
    DB.withConnection { implicit c =>
      val packs = SQL("select * from Pack").apply().map(row => 
        Pack(
          row[String]("email"),
          row[Option[Int]]("status").getOrElse(-1),
          row[Option[String]]("iapid").getOrElse("")
        )
      ).toList
      packs
    }
  }


  def checkPackNameAvaiable(email: String, packName: String): Boolean = {
    packs(email).indexOf(packName) == -1
  }

  def getPictures(email: String, pack: String): List[String] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select pictures from Pack p
          where p.email = {email} and p.pack = {pack};
        """
      ).on("email" -> email, "pack" -> pack).apply().head match {
        case Row(Some(pictures: String)) => pictures.split(",").toList
        case _ => Nil
      }
    }
  }

  def getPicturesByPackName(pack: String): List[String] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select pictures from Pack p
          where p.pack = {pack};
        """
      ).on("pack" -> pack).apply().head match {
        case Row(Some(pictures: String)) => pictures.split(",").toList
        case _ => Nil
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
      val pictureList = (getPictures(email, packName) ++ pictures).mkString(",")
      DB.withConnection { implicit c =>
        SQL(
          """
            update Pack p set pictures = {pictureList}
            where p.email = {email} and p.pack = {packName};
          """
        ).on("pictureList" -> pictureList, "email" -> email, "packName" -> packName).executeUpdate()
      }
    } else {
      val oldPackList = packs(email)
      if(oldPackList.indexOf(packName) == -1) {
        val newPackList = (oldPackList :+ packName).mkString(",")
        DB.withConnection { implicit c =>
          SQL(
            """
              update User u set packs = {packList}
              where u.email = {email};
            """
          ).on("packList" -> newPackList, "email" -> email).executeUpdate()
        }
      }

      val pictureList = pictures.mkString(",")
      DB.withConnection { implicit c =>
        val id: Option[Long] = SQL("insert into Pack(email, pack, pictures) values ({email}, {pack}, {pictures})")
          .on('email -> email, 'pack -> packName, 'pictures -> pictureList).executeInsert()
      }
    }
  }

  def removeIndex(internalIdList: List[String], ix: Int) = {
    if (internalIdList.size < ix) {
      internalIdList
    } else {
      internalIdList.take(ix) ++ internalIdList.drop(ix+1)
    }
  }

  def deletePic(email: String, packName: String, picName: String): Boolean = {
    val old = getPictures(email, packName)
    val index = old.indexOf(picName)
    if(index != -1) {
      val newList = removeIndex(old, index).mkString(",")
      DB.withConnection { implicit c =>
        SQL(
          """
            update Pack p set pictures = {pictureList}
            where p.email = {email} and p.pack = {packName};
          """
        ).on("pictureList" -> newList, "email" -> email, "packName" -> packName).executeUpdate()
      }
      true
    } else {
      false
    }
  }

  def deletePack(email: String, packName: String): Boolean = {
    val old = packs(email)
    val index = old.indexOf(packName)
    if(index != -1) {
      val newList = removeIndex(old, index).mkString(",")
      DB.withConnection { implicit c =>
        SQL(
          """
            update User u set packs = {packList}
            where u.email = {email};
          """
        ).on("packList" -> newList, "email" -> email).executeUpdate()
      }
      DB.withConnection { implicit c =>
        SQL(
          """
            delete from Pack p 
            where p.email = {email} and p.pack = {packName};
          """
        ).on("email" -> email, "packName" -> packName).executeUpdate()
      }
      true
    } else {
      false
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