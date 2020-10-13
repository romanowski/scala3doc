package dotty.dokka
package model
package api

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.{Projection => JProjection}
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.model.DFunction
import org.jetbrains.dokka.model.DClass
import org.jetbrains.dokka.model.DocumentableSource
import org.jetbrains.dokka.model.Dynamic
import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.model.TypeConstructor
import org.jetbrains.dokka.model.TypeParameter
import org.jetbrains.dokka.model.UnresolvedBound

import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc.DocumentationNode
import org.jetbrains.dokka.model.properties._  
import java.util.{List => JList, Set => JSet}

import dokka.java.api.SourceSetWrapper

private [model] case class MemberExtension(
  visibilty: Visibility,
  modifiers: Seq[dotty.dokka.model.api.Modifier],
  kind: Kind,  // = Kind.Uknown,
  val annotations: List[Annotation], //  = Nil
  signature: Signature = Nil, // TODO remove defaults!
  origin: Origin = Origin.DefinedWithin,
  isEnumMember: Boolean = false,
) extends ExtraProperty[Documentable]:
 override def getKey = MemberExtension

object MemberExtension extends BaseKey[Documentable, MemberExtension]:
  val empty = MemberExtension(Visibility.Unrestricted, Nil, Kind.Uknown, Nil)

case class CompositeMemberExtension(
  members : Seq[Member] = Nil,
  parents: Seq[LinkToType] = Nil,
  knownChildren: Seq[Link] = Nil
) extends ExtraProperty[DClass]:
  override def getKey = CompositeMemberExtension

object CompositeMemberExtension extends BaseKey[DClass, CompositeMemberExtension]:
  val empty = CompositeMemberExtension()

type Member = DClass

object Member:
  def apply(
    dri: DRI,
    name: String,
    sourceSet: SourceSetWrapper,
    documentation: Option[DocumentationNode],
    source: DocumentableSource,
    ext: CompositeMemberExtension,
    classlike: Option[CompositeMemberExtension]
  ) = 
    def inSourceSet[A](a:A) = Map(sourceSet.getSourceSet -> a).asJava
    new DClass(
          dri,
          name,
          Nil.asJava,
          Nil.asJava,
          Nil.asJava,
          classlike.fold(Nil)(_.members).filter(_.origin == Origin.DefinedWithin).asJava,
          inSourceSet(source),
          Map.empty.asJava,
          null,
          List.empty.asJava,
          Map.empty.asJava,
          documentation.fold(Map.empty.asJava)(inSourceSet),
          null,
          Map.empty.asJava,
          Set.empty.asJava, // TODO add sourceSet !
          classlike.foldLeft(PropertyContainer.Companion.empty() plus ext)(_ plus _)
      )

extension [E <: Documentable with WithExtraProperties[E]](member: E):
  def copy(modifiers: Seq[Modifier]) =
    val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(modifiers = modifiers)
    member.put(ext)
  
  // def withOrigin(origin: Origin)
  //   val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(origin = origin)
  //   member.put(ext)

extension (bound: Bound):
  def asSignature: Signature = bound match 
    case tc: TypeConstructor =>
      tc.getProjections.asScala.toSeq.map {
        case txt: UnresolvedBound => txt.getName
        case link: TypeParameter =>
          Link(link.getName, link.getDri)
      }