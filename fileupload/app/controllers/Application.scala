package controllers

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

  def index = Action {
    val email = "xxx"
    Ok(views.html.index(email))
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
        User.create(user.email, user.password)
        Redirect(routes.Application.index)
      }
    )
  }

  def signin = Action {
    Ok(views.html.signup(userForm))
  }

  def login = Action {
    // TODO
    Redirect(routes.Application.index)
  }

}