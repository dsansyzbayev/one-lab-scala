package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths
import scala.io.StdIn.readLine


import scala.jdk.CollectionConverters._
import scala.util.chaining._

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(error: String) extends Command
  case class ListDirectoryCommand()           extends Command
  case class ListFilesCommand()               extends Command
  case class ListAllContentCommand()          extends Command
  case class CurrentDirectoryCommand()        extends Command

  case class ChangeDirectoryCommand(val destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class Path(var path: String)

  val currPath = Path("/home")

  case class ChangePathError(error: String)

  def getFiles(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isFile)
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList
  }

  def getDirectories(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isDirectory)
      .map(path => path.toFile.getName())
      .map(name => s"$path/$name")
      .toList
  }

  def getAllContent(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .map(path => path.toFile.getName())
      .map(name => s"$path/$name")
      .toList
  }

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    if (path.equals("..")) {
      val currSplit = current.split("/")
      val newPath = current.substring(0, (current.length() - currSplit(currSplit.length - 1).length - 1))
      currPath.path = newPath
      Right(s"$newPath")
    }
    else {
      val dirs = getDirectories(current)
      if (dirs.contains(s"$current/$path")) {
        currPath.path = s"$current/$path"
        Right(s"$current/$path")
      }
      else {
        Left(new ChangePathError(s"cd: no such file or directory: $path"))
      }
    }
  }

  def getCurrentDirectory(): String = {
    currPath.path
  }

  def parseCommand(input: String): Command = input match {
    case str if str.startsWith("dir") => ListDirectoryCommand()
    case str if str.startsWith("cd") => ChangeDirectoryCommand(str.substring(str.lastIndexOf(" ")+1))
    case str if str.startsWith("ll") => ListAllContentCommand()
    case str if str.startsWith("ls") => ListFilesCommand()
    case str if str.startsWith("pwd") =>CurrentDirectoryCommand()
  }

  def handleCommand(command: Command, currentPath: String): String = command match {
    case ListDirectoryCommand() => getDirectories(currentPath).mkString("\n")
    case ListFilesCommand() => getFiles(currentPath).mkString("\n")
    case ListAllContentCommand() => getAllContent(currentPath).mkString("\n")
    case ChangeDirectoryCommand(destination) => changePath(currentPath, destination).toString
    case CurrentDirectoryCommand() => getCurrentDirectory()
  }

  def main(basePath: String): Unit = {
    println(s"You are in directory -> $basePath")
    while(true) {
      val curr = currPath.path
      val comm = readLine()
      println(handleCommand(parseCommand(comm), curr))
    }

  }
  main(currPath.path)
}
