package dotty.dokka
package model
package api

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  
import org.jetbrains.dokka.pages._
import java.util.{List => JList, Set => JSet}


enum Visibility(val name: String):
  case Unrestricted extends Visibility("")
  case Protected(scope: VisibilityScope) extends Visibility("protected")
  case Private(scope: VisibilityScope) extends Visibility("private")

enum VisibilityScope:
  case ImplicitTypeScope // private/protected inside a class or a trait
  case ImplicitModuleScope // private/protected inside a package or an object
  case ExplicitTypeScope(typeName: String) // private[X]/protected[X] inside a class or a trait
  case ExplicitModuleScope(moduleName: String) // private[X]/protected[X] inside a package or an object
  case ThisScope // private[this]/protected[this]

// TODO probably we can remove prefix
enum Modifier(val name: String, val prefix: Boolean):
  case Sealed extends Modifier("sealed", true)
  case Case extends Modifier("case", false)
  case Implicit extends Modifier("implicit", true)
  case Inline extends Modifier("inline", true)
  case Lazy extends Modifier("lazy", true)
  case Override extends Modifier("override", true)
  case Erased extends Modifier("erased", true)
  case Opaque extends Modifier("opaque", true)
  case Open extends Modifier("open", true)
  
enum Kind(val name: String){
  case Class extends Kind("class")
  case Object extends Kind("object")
  case Trait extends Kind("trait")
  case Enum extends Kind("enum")
  case EnumCase extends Kind("case")
  case Def extends Kind("def")
  case Constructor extends Kind("def")
  case Var extends Kind("var")
  case Val extends Kind("val")
  case Type extends Kind("Type")
  case Given extends Kind("Given")
}

enum Origin:
  case InheritedFrom(name: String, dri: DRI)
  case ImplicitlyAddedBy(name: String, dri: DRI)
  case ExtensionFrom(name: String, dri: DRI)
  case DefinedWithin


case class Annotation(val dri: DRI, val params: List[Annotation.AnnotationParameter])

object Annotation:
  sealed trait AnnotationParameter
  case class PrimitiveParameter(val name: Option[String] = None, val value: String) extends AnnotationParameter
  case class LinkParameter(val name: Option[String] = None, val dri: DRI, val value: String) extends AnnotationParameter
  case class UnresolvedParameter(val name: Option[String] = None, val unresolvedText: String) extends AnnotationParameter

// TODO (longterm) properly represent signatures
type Signature = Seq[String | (String, DRI)]

extension (member: Documentable with WithExtraProperties[_]):
  def visibilty: Visibility = DocumentableExtension.getFrom(member).fold(Visibility.Unrestricted)(_.visibilty)
  def signature: Signature = DocumentableExtension.getFrom(member).fold(Nil)(_.signature)
  def modifiers: Seq[dotty.dokka.model.api.Modifier] = DocumentableExtension.getFrom(member).fold(Nil)(_.modifiers)
  def kind: Kind = DocumentableExtension.getFrom(member).fold(Kind.Def)(_.kind) // throw exception?
  def origin: Origin =  DocumentableExtension.getFrom(member).fold(Origin.DefinedWithin)(_.origin)
  def annotations: List[Annotation] = DocumentableExtension.getFrom(member).fold(Nil)(_.annotations) 

extension (classLike: DClass):
  def allMembers: Seq[Documentable] = ClasslikeExtension.getFrom(classLike).fold(Nil)(_.members)
  def parents: List[String | (String, DRI)] = ClasslikeExtension.getFrom(classLike).fold(Nil)(_.parents)
  def knownChildren: List[(String, DRI)] = ClasslikeExtension.getFrom(classLike).fold(Nil)(_.knownChildren)