package dotty.dokka

import mdoc.internal.cli._
import java.nio.file.Path
import java.nio.file.Files
import mdoc.internal.cli.InputFile
import scala.meta.io._

case class MDocProcessor(cp: String) extends com.virtuslab.dokka.site.PreProcessor:
  val ops = new MainOps(Context.fromOptions(???))


  override def processMarkdown(original: String): String =
    val base = Files.createTempDirectory("tmp-out")
    val inputFile = base.resolve("in").resolve("a.md")
    Files.createDirectory(inputFile.getParet())
    Files.writeString(inputFile, original)

    val out = base.resolve("out")
    Files.createDirectory(out)
    val outFile = out.resolve("a.md")
    
    val input = InputFile(
      RelativePath(inputFile.getFileName),
      AbsolutePath(inputFile),
      AbsolutePath(outFile),
      AbsolutePath(inputFile.getParent),
      AbsolutePath(out)
    )

    ops.handleMarkdown(input)

    Files.readString(outFile)