package dotty.dokka.translators

import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder, PageContentBuilder$DocumentableContentBuilder}
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.doc._
import dotty.dokka.model.api.visibility
import dotty.dokka.model.api.modifiers
import dotty.dokka.model.api.origin
import dotty.dokka.model.api.Origin


import dotty.dokka._

object FilterAttributes:
  def attributesFor(documentable: Documentable): Map[String, String] = 
    val base = visibity(documentable) ++ visibity(documentable) ++ origin(documentable) ++ keywords(documentable)
    base.filter(_._2.nonEmpty)

  private def keywords(documentable: Documentable): Map[String, String] = documentable match 
    case v: WithExtraProperties[_] with WithAbstraction =>
      Map("keywords" -> v.modifiers.map(_.name).mkString(","))  
    case _ =>
      Map.empty


  private def visibity(documentable: Documentable): Map[String, String] = documentable match
    case v: Documentable with WithExtraProperties[_] => 
      Map("visibility" -> v.visibility.name)
    case _ => 
      Map.empty


  private def origin(documentable: Documentable): Map[String, String] =  documentable match
    case v: Documentable with WithExtraProperties[_] => 
      v.origin match
        case Origin.InheritedFrom(name, _) => Map("inherited" -> name)
        case Origin.ImplicitlyAddedBy(name: String, _) => Map("implicitly" -> s"by $name")
        case Origin.ExtensionFrom(name: String, _) => Map("extension" -> s"from $name")
        case _ => Map.empty
    case _ =>
      Map.empty     

  def defaultValues = Map(
    "inherited" ->  "Not inherited",
    "implicitly" -> "Explicit method",
    "extension" -> "Standard member",
    "keywords" -> "no keywords",
    "visibility" -> "public",
  )
