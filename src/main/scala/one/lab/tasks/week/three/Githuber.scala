package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import scala.concurrent.duration._

import scala.concurrent.{
  Await,
  ExecutionContext,
  ExecutionContextExecutor,
  Future
}
import org.json4s.DefaultFormats
import RestClientImpl._
import org.json4s.jackson.JsonMethods.parse

import scala.util.Failure
import scala.util.Success

object Githuber extends App {
  implicit val system: ActorSystem = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer =
    ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(login: String, public_repos: String)
  case class GithubRepository(name: String,
                              size: Int,
                              fork: Boolean,
                              stargazers_count: Int)

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = {
    val response = get(s"https://api.github.com/users/$username")(
      system,
      materializer,
      executionContext)
    response.flatMap { body =>
      Future { parse(body).extract[GithubUser] }
    }
  }

  def getUserRepositories(repoUrl: String): Future[List[GithubRepository]] = {
    val response = get(repoUrl)(system, materializer, executionContext)
    response.flatMap { body =>
      Future { parse(body).extract[List[GithubRepository]] }
    }
  }

  def getUserInfo(username: String): Unit = {

    getGithubUser(username).onComplete {
      case Success(user) =>
        println(s"${user.login} has ${user.public_repos} repositories")
        getUserRepositories(s"https://api.github.com/users/${user.login}/repos").onComplete {
          case Success(repos) => for(repo <- repos) println(s"${repo.name}: has ${repo.stargazers_count} starts, ${repo.size} KB size, repo is ${if (repo.fork) "forked" else "not forked"}")
          case Failure(e) =>println(e.getMessage)
        }
      case Failure(e) => println(e.getMessage)
    }

  }

  getUserInfo("dsansyzbayev")
}
