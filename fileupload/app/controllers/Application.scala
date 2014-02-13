package controllers

import scala.util._

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import models._


object Application extends Controller {
  val userForm = Form(
    mapping(
      "email" -> email,
      "password" -> nonEmptyText(minLength = 3, maxLength = 32)
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
        Ok(views.html.pack(c.value, "new", Nil))
      case None => Redirect(routes.Application.signin)
    }
  }

  def packEdit(pack: String) = Action { implicit request =>
    request.cookies.get("token") match {
      case Some(c) =>
        val pictures = User.getPictures(c.value, pack)
        Ok(views.html.pack(c.value, "edit", pictures))
      case None => Redirect(routes.Application.signin)
    }
  }

  def packSave(pack: String) = Action { implicit request =>
    val email = request.cookies.get("token") match {
      case Some(c) => c.value
      case None => ""
    }
    // save
    Redirect(routes.Application.packEdit(pack))
  }


  def upload = Action(parse.multipartFormData) { request =>
    val email = request.cookies.get("token") match {
      case Some(c) => c.value
      case None => ""
    }
    val packName = "xx"
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

  // def ajaxUpload = Action(parse.temporaryFile) { request =>
  //   val email = request.cookies.get("token") match {
  //     case Some(c) => c.value
  //     case None => ""
  //   }
  //   // val now = System.currentTimeMillis
  //   // val filename = email + '_' + now + picture.filename.substring(picture.filename.lastIndexOf("."))
  //   request.body.moveTo(new File("public/uploadpictures/ajax"))
  //   Ok("File uploaded")
  // }


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

}