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
    val user = Await.result(getGithubUser(username), 5.seconds)
    val userRepos = Await.result(
      getUserRepositories(s"https://api.github.com/users/$username/repos"),
      5.seconds)
    println(s"${user.login} has ${user.public_repos} repositories")
    for (repo <- userRepos)
      println(
        s"${repo.name}: has ${repo.stargazers_count} starts, ${repo.size} KB size, repo is ${if (repo.fork) "forked" else "not forked"}")
  }

  getUserInfo("dsansyzbayev")
}
