package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths
import scala.io.StdIn.readLine
import scala.io.Source

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

  case class PrintErrorCommand(error: String)                 extends Command
  case class ListDirectoryCommand()                           extends Command
  case class ListFilesCommand()                               extends Command
  case class ListAllContentCommand()                          extends Command
  case class CurrentDirectoryCommand()                        extends Command
  case class OutputFileCommand(fileName: String)              extends Command
  case class DeleteFileCommand(fileName: String)              extends Command
  case class CreateNewFileCommand(fileName: String)           extends Command
  case class CreateNewDirectoryCommand(directoryName: String) extends Command
  case class ChangeDirectoryCommand(destination: String)      extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)
  case class FileDoesNotExistError(error: String)

  def getFiles(path: String): List[String] =
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isFile)
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList

  def getDirectories(path: String): List[String] =
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isDirectory)
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList

  def getAllContent(path: String): List[String] =
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    var cur = s"$current/$path"
    if(path.equals("..")) {
      cur = current.split("/").init.mkString("/")
    }

    Files.isDirectory(Paths.get(cur)) match {
      case true  => Right(cur)
      case false => Left(ChangePathError(s"cd: no such file or directory: $path"))
    }
  }

  def createANewFile(filePath: String, fileName: String): String = {
    val path = Paths.get(s"$filePath/$fileName")
    Files.createFile(path)
    s"$fileName was created"
  }

  def deleteFile(filePath: String, fileName: String): Either[FileDoesNotExistError, String] = {
    Files.deleteIfExists(Paths.get(s"$filePath/$fileName")) match {
      case true  => Right(s"$fileName was deleted")
      case false => Left(FileDoesNotExistError("file or directory does not exist"))
    }
  }

  def createDirectory(filePath: String, directoryName: String): String = {
    val path = Paths.get(s"$filePath/$directoryName")
    Files.createDirectory(path)
    s"$directoryName was created"
  }

  def outputFile(filePath: String, fileName: String): String = {
    val source = Source.fromFile(s"$filePath/$fileName")
    try source.mkString finally source.close()
  }

  def parseCommand(input: String): Command = input match {
    case str if str.startsWith("dir")   => ListDirectoryCommand()
    case str if str.startsWith("cd")    => ChangeDirectoryCommand(str.substring(str.lastIndexOf(" ")+1))
    case str if str.startsWith("ll")    => ListAllContentCommand()
    case str if str.startsWith("ls")    => ListFilesCommand()
    case str if str.startsWith("pwd")   => CurrentDirectoryCommand()
    case str if str.startsWith("touch") => CreateNewFileCommand(str.substring(str.lastIndexOf(" ")+1))
    case str if str.startsWith("rm")    => DeleteFileCommand(str.substring(str.lastIndexOf(" ") + 1))
    case str if str.startsWith("mkdir") => CreateNewDirectoryCommand(str.substring(str.lastIndexOf(" ") + 1))
    case str if str.startsWith("cat")   => OutputFileCommand(str.substring(str.lastIndexOf(" ")+1))
  }

  def handleCommand(command: Command, currentPath: String): String = command match {
    case ListDirectoryCommand()                   => getDirectories(currentPath).mkString("\n")
    case ListFilesCommand()                       => getFiles(currentPath).mkString("\n")
    case ListAllContentCommand()                  => getAllContent(currentPath).mkString("\n")
    case CurrentDirectoryCommand()                => currentPath
    case CreateNewFileCommand(fileName)           => createANewFile(currentPath, fileName)
    case CreateNewDirectoryCommand(directoryName) => createDirectory(currentPath, directoryName)
    case OutputFileCommand(fileName)              => outputFile(currentPath, fileName)
    case DeleteFileCommand(fileName)              => deleteFile(currentPath, fileName) match {
      case Left(value)  => value.error
      case Right(value) => value
    }
    case ChangeDirectoryCommand(destination)      => changePath(currentPath, destination) match {
      case Left(value)  => value.error
      case Right(value) => value
    }
  }

  def main(basePath: String): Unit = {
    def innerLoop(currentPath: String): Unit = {
      val in      = readLine()
      val command = parseCommand(in)
      val result  = handleCommand(command, currentPath)

      println(result)

      if (command.isSubstitutive) innerLoop(result)
      else innerLoop(currentPath)

      println("You are in directory ->")
      println(s"$basePath")
      innerLoop(basePath)
    }
    innerLoop(basePath)
  }
  main("/home")
}
