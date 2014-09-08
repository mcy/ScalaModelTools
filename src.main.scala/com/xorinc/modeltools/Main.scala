package com.xorinc.modeltools

import java.io.{FileWriter, FileReader, File}
import com.google.gson.Gson
import com.xorinc.modeltools.tools.{ToolException, Tool}
import org.apache.commons.io.{FileUtils, IOUtil}
import scopt._
import scala.collection.mutable

object Main {

  type In = java.io.InputStream
  type Out = java.io.OutputStream
  type FIn = java.io.FileInputStream
  type FOut = java.io.FileOutputStream
  type IOE = java.io.IOException

  private var _inFile, _outFile: File = _

  private val gson = new Gson()

  def inFile = _inFile
  def outFile = _outFile

  private lazy val parser =
    new OptionParser[Config]("modelTools") {

      head("modelTools", "${version}")

      help("help") text "prints this help message"
      version("version") text "prints the current version"

      opt[Unit]("debug") action { (b, c) =>
        c.copy(debug = true)
      } text "debug output" maxOccurs 1 hidden()

      opt[File]('o', "out") action { (f, c) =>
        c.copy(output = ConstOutput(f))
      } text "file output (input will be used as output if omitted)" maxOccurs 1

      opt[String]('s', "suffix") action { (s, c) =>
        c.copy(output = SuffixOutput(s))
      } text "file output prefix (overrides output)" maxOccurs 1

      opt[String]('i', "indent") action { (s, c) =>
        if(s.matches(" *") || s.matches("\t*"))
          c.copy(indent = s)
        else c
      }

      for((k, v) <- Tool.tools)
        v.register(this, cmd(k) action { (_, c) =>
          c.copy(tool = v)
        } text v.desc)

      arg[File]("<file>...") unbounded() required() action { (x, c) =>
        c.copy(inputs = c.inputs :+ x) } text "inputs"

      override val showUsageOnError = true
    }

  private var conf: Option[Config] = None

  def main(arg: Array[String]): Unit = {

    conf = parser.parse(
      if(Console.in.ready())
        arg ++ IOUtil.toString(Console.in).split("\n")
      else
        arg,
      Config()
    )

    conf match {
      case None => return
      case Some(Config(tool, _debug, in, out, opt, _)) =>

        if(tool == null){
          System.err.println("No tool selected!")
          parser.showUsageAsError
          return
        }

        in map ( f => (f, out(f)) ) foreach { t =>

          _inFile = t._1
          _outFile = t._2

          var s = IOUtil.toString(new FileReader(_inFile))
          debug(s)
          try {
            s = tool(s, tool.parse(opt.toMap))
          } catch {
            case ToolException(message, cause) =>
              Console.err.println(s"Error applying tool: $message")
              cause match {
                case Some(x) if _debug => x.printStackTrace()
                case _ =>
              }
            return
          }
          debug(s)
          debug(_outFile)
          FileUtils.fileWrite(_outFile, s)
        }

    }

  }

  sealed abstract class Output {
    def apply(f: File): File
  }

  object SelfOutput extends Output {
    override def apply(f: File) = f
  }

  final case class ConstOutput(o: File) extends Output {
    override def apply(f: File) = o
  }

  final case class SuffixOutput(suf: String) extends Output {
    override def apply(f: File) = {
      import FileUtils._
      new File(removeExtension(f) + suf + "." + getExtension(f))
    }
  }

  case class Config
    (
      tool: Tool = null,
      debug: Boolean = false,
      inputs: List[File] = Nil,
      output: Output = SelfOutput,
      options: mutable.Map[String, Any] = mutable.HashMap.empty[String, Any],
      indent: String = " " * 4
    )

  def debug(message: => String): Unit =
    conf match {
      case Some(x) if x.debug =>
        Console.err.println(message)
      case None =>
    }

  implicit def fromFile(f: File): String = f.getName
  implicit def toFile(s: String): File = new File(s)
  def opts: Config = conf.getOrElse(Config())
}