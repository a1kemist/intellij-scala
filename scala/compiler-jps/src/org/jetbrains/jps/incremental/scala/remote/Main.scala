package org.jetbrains.jps.incremental.scala.remote

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util
import java.util.{Base64, Timer, TimerTask}

import com.martiansoftware.nailgun.NGContext
import org.jetbrains.jps.api.{BuildType, CmdlineProtoUtil}
import org.jetbrains.jps.builders.java.JavaModuleBuildTargetType
import org.jetbrains.jps.cmdline.{BuildRunner, JpsModelLoaderImpl}
import org.jetbrains.jps.incremental.MessageHandler
import org.jetbrains.jps.incremental.fs.BuildFSState
import org.jetbrains.jps.incremental.messages.{BuildMessage, CustomBuilderMessage, ProgressMessage}
import org.jetbrains.jps.incremental.scala.Client
import org.jetbrains.jps.incremental.scala.data.CompileServerCommandParser
import org.jetbrains.jps.incremental.scala.local.LocalServer
import org.jetbrains.jps.incremental.scala.local.worksheet.WorksheetServer
import org.jetbrains.plugins.scala.compiler.CompilerEvent
import org.jetbrains.plugins.scala.compiler.data.Arguments
import org.jetbrains.plugins.scala.server.CompileServerToken

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.{Failure, Success, Try}

/**
 * Nailgun Nail, used in:
 *
 * @see [[org.jetbrains.plugins.scala.nailgun.NailgunRunner]]<br>
 *      [[org.jetbrains.plugins.scala.nailgun.NailgunMainLightRunner]]
 *      [[org.jetbrains.plugins.scala.compiler.NonServerRunner]]
 */
object Main {
  private val server = new LocalServer()
  private val worksheetServer = new WorksheetServer

  private var shutdownTimer: Timer = _
  
  /**
   * This method is called by NGServer
   *
   * @see [[http://www.martiansoftware.com/nailgun/quickstart.html]]<br>
   *      [[http://www.martiansoftware.com/nailgun/doc/javadoc/com/martiansoftware/nailgun/NGContext.html]]<br>
   *      [[com.martiansoftware.nailgun.NGContext]]<br>
   *      [[com.martiansoftware.nailgun.NGSession:153]]<br>
   *      [[com.martiansoftware.nailgun.NGServer:198]]<br>
   */
  def nailMain(context: NGContext): Unit = {
    cancelShutdownTimer()
    serverLogic(
      commandId = context.getCommand,
      argsEncoded = context.getArgs.toSeq,
      out = context.out,
      port = context.getNGServer.getPort,
      standalone = false
    )
    resetShutdownTimer(context)
  }

  def main(args: Array[String]): Unit = {
    serverLogic(CommandIds.Compile, args, System.out, -1, standalone = true)
  }

  private def serverLogic(commandId: String,
                          argsEncoded: Seq[String],
                          out: PrintStream,
                          port: Int,
                          standalone: Boolean): Unit = {
    val client = new EncodingEventGeneratingClient(out, standalone)
    val oldOut = System.out
    // Suppress any stdout data, interpret such data as error
    System.setOut(System.err)


    try {
      val command = parseArgs(commandId, argsEncoded) match {
        case Success(result) =>
          result
        case Failure(error) =>
          client.trace(error)
          return
      }

      // Don't check token in non-server mode
      if (port != -1) {
        try {
          val tokenPath = CompileServerToken.tokenPathForPort(port)
          validateToken(tokenPath, command.token)
        } catch {
          // We must abort the process on _any_ error
          case e: Throwable =>
            client.error(e.getMessage)
            return
        }
      }

      handleCommand(command, client)
    } catch {
      case e: Throwable =>
        client.trace(e)
    } finally {
      client.processingEnd()
      client.close()
      System.setOut(oldOut)
    }
  }

  private def handleCommand(command: CompileServerCommand, client: EncodingEventGeneratingClient): Unit =
    command match {
      case CompileServerCommand.Compile(arguments) =>
        compileLogic(arguments, client)
      case compileJps: CompileServerCommand.CompileJps =>
        compileJpsLogic(compileJps, client)
    }

  private def compileLogic(args: Arguments, client: EncodingEventGeneratingClient): Unit = {
    val worksheetArgs = args.worksheetArgs
    if (!worksheetArgs.exists(_.isRepl)) {
      server.compile(args.sbtData, args.compilerData, args.compilationData, client)
    }

    worksheetArgs match {
      case Some(wa) if !client.hasErrors =>
        worksheetServer.loadAndRun(wa, args, client)
      case _ =>
    }
  }

  private def compileJpsLogic(command: CompileServerCommand.CompileJps, client: Client): Unit = {
    val CompileServerCommand.CompileJps(_, projectPath, globalOptionsPath, dataStorageRootPath, testScopeOnly, forceCompileModule) = command
    val dataStorageRoot = new File(dataStorageRootPath)
    val loader = new JpsModelLoaderImpl(projectPath, globalOptionsPath, false, null)
    val buildRunner = new BuildRunner(loader)
    var compiledFiles = Set.empty[File]
    val messageHandler = new MessageHandler {
      override def processMessage(msg: BuildMessage): Unit = msg match {
        case customMessage: CustomBuilderMessage =>
          CompilerEvent.fromCustomMessage(customMessage).foreach {
            case CompilerEvent.MessageEmitted(_, msg) => client.message(msg)
            case CompilerEvent.CompilationFinished(_, sources) => compiledFiles ++= sources
            case _ => ()
          }
        case progressMessage: ProgressMessage =>
          val text = Option(progressMessage.getMessageText).getOrElse("")
          val done = Option(progressMessage.getDone).filter(_ >= 0.0)
          client.progress(text, done)
        case _ =>
          ()
      }
    }
    val descriptor = buildRunner.load(messageHandler, dataStorageRoot, new BuildFSState(true))
    val forceBuild = false
    val scopeTypes = if (testScopeOnly)
      Seq(JavaModuleBuildTargetType.TEST)
    else
      Seq(JavaModuleBuildTargetType.TEST, JavaModuleBuildTargetType.PRODUCTION)
    val scopes = scopeTypes.map(CmdlineProtoUtil.createAllTargetsScope(_, forceBuild)).asJava

    client.compilationStart()
    try {
      buildRunner.runBuild(
        descriptor,
        () => client.isCanceled,
        null,
        messageHandler,
        BuildType.BUILD,
        scopes,
        true
      )
      // TODO improve. Force compile only one file. And only if it wasn't compiled in previous compilation.
      forceCompileModule.foreach { module =>
        val scopes = scopeTypes.map { scopeType =>
          CmdlineProtoUtil.createTargetsScope(scopeType.getTypeId, util.Arrays.asList(module), forceBuild)
        }.asJava
        buildRunner.runBuild(
          descriptor,
          () => client.isCanceled,
          null,
          messageHandler,
          BuildType.BUILD,
          scopes,
          false
        )
      }
    } finally {
      client.compilationEnd(compiledFiles)
      descriptor.release()
    }
  }
  
  private def parseArgs(command: String, argsEncoded: Seq[String]): Try[CompileServerCommand] = {
    val args = argsEncoded.map(decodeArgument)
    CompileServerCommandParser.parse(command, args)
  }

  private def decodeArgument(argEncoded: String): String = {
    val decoded = Base64.getDecoder.decode(argEncoded.getBytes)
    val str = new String(decoded, StandardCharsets.UTF_8)
    if (str == "#STUB#") "" else str
  }

  @throws(classOf[TokenVerificationException])
  private def validateToken(path: Path, actualToken: String): Unit = {
    if (!path.toFile.exists) {
      throw new TokenVerificationException("Token not found: " + path)
    }

    val expectedToken = try {
      new String(Files.readAllBytes(path))
    } catch {
      case _: IOException =>
        throw new TokenVerificationException("Cannot read token: " + path)
    }

    if (!expectedToken.equals(actualToken)) {
      throw new TokenVerificationException("Token is incorrect: " + actualToken)
    }
  }

  private class TokenVerificationException(message: String) extends Exception(message)

  private def cancelShutdownTimer(): Unit = synchronized {
    if (shutdownTimer != null) {
      shutdownTimer.cancel()
      shutdownTimer = null
    }
  }

  private def resetShutdownTimer(context: NGContext): Unit = {
    val delay = Option(System.getProperty("shutdown.delay")).map(_.toInt)
    delay.foreach { t =>
      val delayMs = t * 60 * 1000
      val shutdownTask = new TimerTask {
        override def run(): Unit = context.getNGServer.shutdown(true)
      }

      synchronized {
        cancelShutdownTimer()
        shutdownTimer = new Timer()
        shutdownTimer.schedule(shutdownTask, delayMs)
      }
    }
  }
}
