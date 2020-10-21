package dotty.dokka
package tasty

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.properties.PropertyContainerKt._
import org.jetbrains.dokka.model.properties.{WithExtraProperties}
import java.util.{List => JList}

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.dokka.model.api.withNewMembers
import com.virtuslab.dokka.site.SourceSetWrapper

/** Responsible for collectively inspecting all the Tasty files we're interested in.
  *
  * Delegates most of the work to [[TastyParser]] [[dotty.dokka.tasty.TastyParser]].
  */
case class DokkaTastyInspector(sourceSet: SourceSetWrapper, parser: Parser, config: DottyDokkaConfig) extends DokkaBaseTastyInspector with TastyInspector

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
case class SbtDokkaTastyInspector(
  sourceSet: SourceSetWrapper,
  config: DottyDokkaConfig,
  filesToDocument: List[String],
  rootCtx: DottyContext,
) extends DokkaBaseTastyInspector:
  self =>

  import dotty.tools.dotc.Compiler
  import dotty.tools.dotc.Driver
  import dotty.tools.dotc.Run
  import dotty.tools.dotc.core.Contexts.Context
  import dotty.tools.dotc.core.Mode
  import dotty.tools.dotc.core.Phases.Phase
  import dotty.tools.dotc.fromtasty._
  import dotty.tools.dotc.quoted.QuoteContextImpl

  val parser: Parser = null

  def run(): List[DPackage] = {
    val driver = new InspectorDriver
    driver.run(filesToDocument)(rootCtx)
    result()
  }

  class InspectorDriver extends Driver:
    override protected def newCompiler(implicit ctx: Context): Compiler = new TastyFromClass

    def run(filesToDocument: List[String])(implicit ctx: Context): Unit =
      doCompile(newCompiler, filesToDocument)

  end InspectorDriver

  class TastyFromClass extends TASTYCompiler:

    override protected def frontendPhases: List[List[Phase]] =
      List(new ReadTasty) :: // Load classes from tasty
      Nil

    override protected def picklerPhases: List[List[Phase]] = Nil

    override protected def transformPhases: List[List[Phase]] = Nil

    override protected def backendPhases: List[List[Phase]] =
      List(new TastyInspectorPhase) ::  // Print all loaded classes
      Nil

    override def newRun(implicit ctx: Context): Run =
      reset()
      new TASTYRun(this, ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))

  end TastyFromClass

  class TastyInspectorPhase extends Phase:

    override def phaseName: String = "tastyInspector"

    override def run(implicit ctx: Context): Unit =
      val qctx = QuoteContextImpl()
      self.processCompilationUnit(qctx.tasty)(ctx.compilationUnit.tpdTree.asInstanceOf[qctx.tasty.Tree])

  end TastyInspectorPhase

end SbtDokkaTastyInspector

trait DokkaBaseTastyInspector:
  val sourceSet: SourceSetWrapper
  val parser: Parser
  val config: DottyDokkaConfig

  private val topLevels = Seq.newBuilder[Documentable]

  def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    val parser = new TastyParser(reflect, this, config)
    topLevels ++= parser.parseRootTree(root.asInstanceOf[parser.reflect.Tree])

  def result(): List[DPackage] =
    val all = topLevels.result()
    val packages = all
      .filter(_.isInstanceOf[DPackage])
      .map(_.asInstanceOf[DPackage])
      .groupBy(_.getDri)
      .map((dri, pckgs) =>
        pckgs.reduce(_.mergeWith(_))
      )

    val byPackage = all.filter(_.getDri != null).groupBy(_.getDri().getPackageName())
    byPackage.map {
      case (pck, entries) => {
        val found = packages.find(d => d.getName == pck)
        .map( f =>
          new DPackage(
            f.getDri,
            f.getFunctions,
            f.getProperties,
            Nil.asJava, // TODO add support for other things like type or package object entries
            Nil.asJava,
            f.getDocumentation,
            null,
            sourceSet.toSet,
            f.getExtra
          ).withNewMembers(entries.filterNot(_.isInstanceOf[DPackage]).toList).asInstanceOf[DPackage]
        )
        found.getOrElse(throw IllegalStateException("No package for entries found"))
      }
    }.toList

  extension (self: DPackage) def mergeWith(other: DPackage): DPackage =
    val doc1 = self.getDocumentation.asScala.get(sourceSet.getSourceSet).map(_.getChildren).getOrElse(Nil.asJava)
    val doc2 = other.getDocumentation.asScala.get(sourceSet.getSourceSet).map(_.getChildren).getOrElse(Nil.asJava)
    mergeExtras(
      DPackage(
        self.getDri,
        (self.getFunctions.asScala ++ other.getFunctions.asScala).asJava,
        (self.getProperties.asScala ++ other.getProperties.asScala).asJava,
        Nil.asJava, // WARNING Merging is done before collecting classlikes, if it changes it needs to be refactored
        Nil.asJava,
        sourceSet.asMap(
            DocumentationNode(
                (
                  doc1.asScala ++ doc2.asScala
                ).asJava
            )
        ),
        null,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      ),
      self, 
      other
    )

/** Parses a single Tasty compilation unit. */
case class TastyParser(reflect: Reflection, inspector: DokkaBaseTastyInspector, config: DottyDokkaConfig)
    extends ScaladocSupport with BasicSupport with TypesSupport with ClassLikeSupport with SyntheticsSupport with PackageSupport:
  import reflect._

  def sourceSet = inspector.sourceSet

  def processTree[T](tree: Tree)(op: => T): Option[T] = try Option(op) catch case e: Throwable => errorMsg(tree, tree.symbol.show, e)
  def processTreeOpt[T](tree: Tree)(op: => Option[T]): Option[T] = try op catch case e: Throwable => errorMsg(tree, tree.symbol.show, e)
  def processSymbol[T](sym: Symbol)(op: => T): Option[T] = try Option(op) catch case e: Throwable => errorMsg(sym, sym.show, e)

  private def errorMsg[T](a: Any, m: => String, e: Throwable): Option[T] =
    val msg = try m catch case e: Throwable => a.toString
    println(s"ERROR: tree is faling: msg")
    e.printStackTrace()
    throw e

  def parseRootTree(root: Tree): Seq[Documentable] =
    val docs = Seq.newBuilder[Documentable]
    object Traverser extends TreeTraverser:
      var seen: List[Tree] = Nil

      override def traverseTree(tree: Tree)(using ctx: Context): Unit =
        seen = tree :: seen
        tree match {
          case pck: PackageClause =>
            docs += parsePackage(pck)
            super.traverseTree(tree)
          case packageObject: ClassDef if(packageObject.symbol.name.contains("package$")) =>
            docs += parsePackageObject(packageObject)
          case clazz: ClassDef if clazz.symbol.shouldDocumentClasslike =>
            docs += parseClasslike(clazz)
          case _ =>
        }
        seen = seen.tail

    try Traverser.traverseTree(root)(using reflect.rootContext)
    catch case e: Throwable =>
      println(s"Problem parsing ${root.pos}, documentation may not be generated.")
      e.printStackTrace()

    docs.result()

