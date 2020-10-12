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

import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  
import java.util.{List => JList, Set => JSet}

import dokka.java.api.SourceSetWrapper

private [model] case class DocumentableExtension(
  visibilty: Visibility,
  signature: Signature,
  modifiers: Seq[dotty.dokka.model.api.Modifier],
  kind: Kind,
  origin: Origin = Origin.DefinedWithin,
  isEnumMember: Boolean,
  val annotations: List[Annotation]
) extends ExtraProperty[Documentable]:
 override def getKey = DocumentableExtension

object DocumentableExtension extends BaseKey[Documentable, DocumentableExtension]  

case class ClasslikeExtension(
  members : Seq[Member] = Nil,
  parents: List[String | (String, DRI)] = Nil,
  knownChildren: List[(String, DRI)] = Nil
) extends ExtraProperty[DClass]:
  override def getKey = ClasslikeExtension

object ClasslikeExtension extends BaseKey[DClass, ClasslikeExtension]  

type Member = DClass

object Member:
  def apply(
    dri: DRI,
    name: String,
    sourceSet: SourceSetWrapper,
    documentation: Option[DocumentationNode],
    source: DocumentableSource,
    ext: DocumentableExtension,
    classlike: Option[ClasslikeExtension]
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