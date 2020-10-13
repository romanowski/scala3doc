package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.properties._

import dotty.dokka.model._

class InheritanceInformationTransformer(val ctx: DokkaContext) extends DocumentableTransformer{
    override def invoke(original: DModule, context: DokkaContext): DModule = {
        val supertypes = getSupertypes(original)
        val subtypes = getSubtypesMap(supertypes)
        completeInheritanceInformation(subtypes)(original)
    }

    private def getSupertypes(d: Documentable): List[(DRI, DRI)] = d match {
        case m: DModule => m.getPackages.asScala.toList.flatMap(p => getSupertypes(p))
        case p: DPackage => p.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case c: DClass => c.get(InheritanceInfo).parents.map(p => (c.getDri, getTypeDRI(p))) ++ c.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case other => List.empty
    }

    private def getTypeDRI(b: Bound) = b match {
        case t: TypeConstructor => t.getDri
        case other => throw IllegalStateException(s"Supertype without DRI: $b")
    }

    private def getSubtypesMap(supertypesList: List[(DRI, DRI)]): Map[DRI, List[DRI]] = supertypesList
        .map( (a,b) => (b,a) )
        .groupBy( (a,b) => a )
        .map{
            case (key, l) => (key, l.map(_(1)))
        }.toMap

    private def completeInheritanceInformation[T <: Documentable](subtypes: Map[DRI, List[DRI]])(d: T): T = (d match {
        case m: DModule => 
            m.updatePackanges(_.map(completeInheritanceInformation(subtypes)))
        
        case p: DPackage => 
            p.updateClasslikes(_.map(completeInheritanceInformation(subtypes)))
        
        case c: DClass => 
            val newInheritanceInfo = InheritanceInfo(
                c.get(InheritanceInfo).parents,
                subtypes.get(c.getDri).getOrElse(List.empty)
            )
            c.updateClasslikes(_.map(completeInheritanceInformation(subtypes))).put(newInheritanceInfo)

        case other => other
    }).asInstanceOf[T]

    private def modifyExtras(p: PropertyContainer[DClass], i: InheritanceInfo): PropertyContainer[DClass] = 
        val properties = p.getMap.asScala.toMap.filter( (key,value) => key != InheritanceInfo ).map((key, value) => value).asJavaCollection
        PropertyContainer.Companion.empty addAll properties plus i

}