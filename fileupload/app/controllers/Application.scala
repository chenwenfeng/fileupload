package controllers

import scala.util._

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._

import models._


object Application extends Controller {
  val userForm = Form(
    mapping(
      "email" -> text,
      "password" -> text
    )(User.apply)(User.unapply)
  )


  def index = Action { implicit request =>
    request.cookies.get("token") match {
      case Some(c) =>
        val packs = User.packs(c.value)
        Ok(views.html.index(c.value, packs))
      case None => Redirect(routes.Application.signin)
    }
  }

  def packNew = Action {implicit request =>
    request.cookies.get("token") match {
      case Some(c) =>
        Ok(views.html.pack(c.value, "new", "", Nil))
      case None => Redirect(routes.Application.signin)
    }
  }

  def packEdit(pack: String) = Action { implicit request =>
    request.cookies.get("token") match {
      case Some(c) =>
        val pictures = User.getPictures(c.value, pack)
        Ok(views.html.pack(c.value, "edit", pack, pictures))
      case None => Redirect(routes.Application.signin)
    }
  }

  def upload(packName: String) = Action(parse.multipartFormData) { request =>
    val email = request.cookies.get("token") match {
      case Some(c) => c.value
      case None => ""
    }
    if(email != "") {
      val now = System.currentTimeMillis
      val pictureNames = (for(picture <- request.body.files.toArray) yield {
        import java.io.File
        val random = Random.nextInt(10000)
        val filename = email + '_' + now + '_' + random + picture.filename.substring(picture.filename.lastIndexOf("."))
        val contentType = picture.contentType
        picture.ref.moveTo(new File(s"public/uploadpictures/$filename"))
        filename
      }).toList
      if(pictureNames.size == 0) {
        Redirect(routes.Application.packEdit(packName)).flashing(
          "error" -> "Missing file")
      } else {
        User.updatePack(email, packName, pictureNames)
        Redirect(routes.Application.packEdit(packName))
      }
    } else {
      Redirect(routes.Application.signin)
    }
      
  }

  def deletePic(email: String, packName: String, picName: String) = Action {
    Ok(Json.obj("result" -> User.deletePic(email, packName, picName)))
  }

  def deletePack(email: String, packName: String) = Action {
    Ok(Json.obj("result" -> User.deletePack(email, packName)))
  }


  def users = Action {
    Ok(views.html.users(User.all))
  }

  def signup = Action {
    Ok(views.html.signup(userForm))
  }

  def createUser = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.signup(errors)),
      user => {
        if(User.checkAvaiable(user.email)) {
          User.create(user.email, user.password)
          Redirect(routes.Application.index).withCookies(Cookie("token", user.email))
        } else {
          Ok(views.html.signup(userForm))
        }
      }
    )
  }

  def signin = Action {
    Ok(views.html.signin(userForm))
  }

  def login = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.signin(errors)),
      user => {
        if(User.check(user.email, user.password)) {
          Redirect(routes.Application.index).withCookies(Cookie("token", user.email))
        } else {
          Ok(views.html.signin(userForm))
        }
      }
    )
  }

  implicit val packFormat = Json.format[Pack]

  def apiPacks = Action {
    val packs = User.getAllPacks
    Ok(Json.toJson(packs))
  }

  def apiPictures(packName: String) = Action {
    val pictures = User.getPicturesByPackName(packName)
    Ok(Json.toJson(pictures))
  }

}