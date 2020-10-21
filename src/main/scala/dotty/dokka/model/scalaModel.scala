package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  
import org.jetbrains.dokka.pages._
import java.util.{List => JList, Set => JSet}
import dotty.dokka.model.api.Signature
import dotty.dokka.model.api.HierarchyDiagram

case class TastyDocumentableSource(val path: String, val lineNumber: Int) extends DocumentableSource {
    override def getPath = path
}

enum TableStyle extends org.jetbrains.dokka.pages.Style:
  case Borderless
  case DescriptionList
  case NestedDescriptionList

case class HtmlContentNode(
  val body: String, 
  val dci: DCI, 
  val sourceSets: Set[DisplaySourceSet], 
  val style: Set[Style],
  val extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty
) extends ContentNode:
  override def getDci = dci
  override def getSourceSets = sourceSets.asJava
  override def getStyle = style.asJava
  override def hasAnyContent = !body.isEmpty
  def withSourceSets(sourceSets: JSet[DisplaySourceSet]) = copy(sourceSets = sourceSets.asScala.toSet)
  override def getChildren: JList[ContentNode] = Nil.asJava
  override def getExtra = extra
  override def withNewExtras(p: PropertyContainer[ContentNode]) = copy(extra = p)

class ScalaTagWrapper(root: DocTag) extends TagWrapper(null):
  override def getRoot = root

object ScalaTagWrapper {

  case class See(root: DocTag) extends ScalaTagWrapper(root)
  case class Todo(root: DocTag) extends ScalaTagWrapper(root)
  case class Note(root: DocTag) extends ScalaTagWrapper(root)
  case class Example(root: DocTag) extends ScalaTagWrapper(root)
  case class NestedNamedTag(
    name: String,
    subname: String,
    identTag: DocTag,
    descTag: DocTag
  ) extends NamedTagWrapper(null):
    override def getName = name
    override def getRoot = descTag
}

case class ImplicitConversion(conversion: Documentable, from: DRI, to: DRI)

case class HierarchyDiagramContentNode(
  val diagram: HierarchyDiagram, 
  val dci: DCI, 
  val sourceSets: Set[DisplaySourceSet], 
  val style: Set[Style],
  val extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty
) extends ContentNode:
  override def getDci = dci
  override def getSourceSets = sourceSets.asJava
  override def getStyle = style.asJava
  override def hasAnyContent = !diagram.edges.isEmpty
  def withSourceSets(sourceSets: JSet[DisplaySourceSet]) = copy(sourceSets = sourceSets.asScala.toSet)
  override def getChildren: JList[ContentNode] = Nil.asJava
  override def getExtra = extra
  override def withNewExtras(p: PropertyContainer[ContentNode]) = copy(extra = p)

case class ContentNodeParams(
  val dci: DCI, 
  val sourceSets: java.util.Set[DisplaySourceSet], 
  val style: Set[Style],
  val extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty
):
  def dri = dci.getDri.asScala.head

abstract class ScalaContentNode(params: ContentNodeParams) extends ContentNode:
  def newInstance(params: ContentNodeParams): ScalaContentNode

  override def getDci = params.dci
  override def getSourceSets = params.sourceSets
  override def getStyle = params.style.asJava
  override def hasAnyContent = true
  def withSourceSets(sourceSets: JSet[DisplaySourceSet]) = 
    newInstance(params.copy(sourceSets = sourceSets))
  override def getChildren: JList[ContentNode] = Nil.asJava
  override def getExtra = params.extra
  override def withNewExtras(p: PropertyContainer[ContentNode]) = newInstance(params.copy(extra = p))
   
case class DocumentableElement(
  annotations: Signature,
  modifiers: Signature,
  name: String,
  signature: Signature,
  brief: Seq[ContentNode],
  originInfo: Signature,
  attributes: Map[String, String],
  params: ContentNodeParams
) extends ScalaContentNode(params):
  override def newInstance(params: ContentNodeParams) = copy(params = params)

case class DocumentableElementGroup(
  header: Signature,
  elements: Seq[DocumentableElement],
  params: ContentNodeParams
) extends ScalaContentNode(params):
  override def newInstance(params: ContentNodeParams) = copy(params = params)
  override def hasAnyContent = elements.nonEmpty
  override def getChildren: JList[ContentNode] = elements.asJava

case class DocumentableList(
  groupName: Signature,
  elements: Seq[DocumentableElement | DocumentableElementGroup], 
  params: ContentNodeParams
) extends ScalaContentNode(params):
  override def newInstance(params: ContentNodeParams) = copy(params = params)
  override def hasAnyContent = elements.nonEmpty
  override def getChildren: JList[ContentNode] = elements.asJava

case class DocumentableFilter(params: ContentNodeParams) extends ScalaContentNode(params):
  override def newInstance(params: ContentNodeParams) = copy(params = params)
