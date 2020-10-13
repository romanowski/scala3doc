package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.properties._

import dotty.dokka.model._
import dotty.dokka.model.api._


class InheritanceInformationTransformer(val ctx: DokkaContext) extends DocumentableTransformer:
    override def invoke(original: DModule, context: DokkaContext): DModule = {
        val subtypes = getSupertypes(original).groupBy(_._1).transform((k, v) => v.map(_._2))
        completeInheritanceInformation(subtypes)(original)
    }

    private def getSupertypes(d: Documentable): Seq[(DRI, Link)] = d match {
        case m: DModule => m.getPackages.asScala.toList.flatMap(p => getSupertypes(p))
        case p: DPackage => p.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case c: DClass => 
            val selfLink = Link(c.name, c.dri)
            c.parents.map(_._2 -> selfLink) ++ c.getClasslikes.asScala.flatMap(getSupertypes)
        case other => List.empty
    }

    private def completeInheritanceInformation[T <: Documentable](subtypes: Map[DRI, Seq[Link]])(d: T): T = (d match {
        case m: DModule => 
            m.updatePackanges(_.map(completeInheritanceInformation(subtypes)))
        
        case p: DPackage => 
            p.updateClasslikes(_.map(completeInheritanceInformation(subtypes)))
        
        case c: DClass => 
            val original = CompositeMemberExtension.getFrom(c).getOrElse(CompositeMemberExtension.empty)
            val newInheritanceInfo = original.copy(knownChildren = subtypes.get(c.dri).getOrElse(Nil))
            c.updateClasslikes(_.map(completeInheritanceInformation(subtypes))).put(newInheritanceInfo)
        case other => other
    }).asInstanceOf[T]
