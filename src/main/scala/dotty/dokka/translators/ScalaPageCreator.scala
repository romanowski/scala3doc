package dotty.dokka

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.links.DRIKt.getParent
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.kind

class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger) {

    private val contentBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

    override def pageForModule(m: DModule): ModulePageNode = super.pageForModule(m)

    override def pageForPackage(p: DPackage): PackagePageNode = {
        val page = super.pageForPackage(p)

        val ext = Option(p.get(PackageExtension))
        val extensionPages = ext.fold(
                List.empty
            )(
                _.extensions.flatMap(_.extensions)
                .map(pageForFunction(_))
                .map(page =>
                    page.modified(
                        "extension_" + page.getName,
                        page.getChildren
                    )
                )
            )

        page.modified(
            page.getName,
            (page.getChildren.asScala ++ extensionPages).asJava
        )
    }

    override def pageForClasslike(c: DClasslike): ClasslikePageNode = c match {
            case clazz: DClass => pageForDClass(clazz)
            case other => throw UnsupportedOperationException("Only DClass classlike is supported.")
        }

    def pageForDClass(c: DClass): ClasslikePageNode = {
        val constructors = c.getConstructors

        val ext = c.get(ClasslikeExtension)

        val name = if c.kind == Kind.Object && ext.companion.isDefined then c.getName + "$" else c.getName

        val extensionPages = ext.extensions.flatMap(_.extensions)
            .map(pageForFunction(_))
            .map(page =>
                page.modified(
                    "extension_" + page.getName,
                    page.getChildren
                )
            )
        val enumEntryPages = Option(c.get(EnumExtension)).map(_.enumEntries).collect{
            case c: DClasslike => pageForClasslike(c)
        }

        ClasslikePageNode(
            name,
            contentForClasslike(c),
            JSet(c.getDri),
            c,
            (constructors.asScala.map(pageForFunction) ++
            c.getClasslikes.asScala.map(pageForClasslike) ++
            c.getFunctions.asScala.map(pageForFunction) ++
            enumEntryPages ++ extensionPages).asJava,
            List.empty.asJava
        )

    }

    override def pageForFunction(f: DFunction) = super.pageForFunction(f)

    override def contentForModule(m: DModule) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover) { gbuilder => gbuilder
                .cover(m.getName)()
                .descriptionIfNotEmpty(m)
            }
            .addChildren(contentForComments(m).asScala.toSeq)
            .groupingBlock(
                "Packages",
                List("" -> m.getPackages.asScala.toList),
                kind = ContentKind.Packages,
                sourceSets = m.getSourceSets.asScala.toSet
            )(
                (bdr, elem) => bdr
            ) { (bdr, elem) => bdr
                .driLink(elem.getName, elem.getDri)
            }
            
        contentBuilder.contentForDocumentable(m, buildBlock = buildBlock)
    }

    override def contentForPackage(p: DPackage) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover) { gbuilder => gbuilder
                .cover(p.getName)()
                .descriptionIfNotEmpty(p)
            }
            .group(styles = Set(ContentStyle.TabbedContent)) { b => b
                .contentForScope(p)
            }
        
        contentBuilder.contentForDocumentable(p, buildBlock = buildBlock)
    }

    override def contentForClasslike(c: DClasslike) = c match {
        case d: DClass => contentForClass(d)
        case other => throw UnsupportedOperationException("Only DClass classlike is supported.")
    }

    def contentForClass(c: DClass) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover, sourceSets = c.getSourceSets.asScala.toSet) { gbdr => gbdr
                .cover(c.getName)()
                .sourceSetDependentHint(Set(c.getDri), c.getSourceSets.asScala.toSet) { sbdr => sbdr
                    .signature(c)
                    .contentForDescription(c)
                }
            }
            .documentableFilter()
            .group(styles = Set(ContentStyle.TabbedContent)) { b => b
                .contentForScope(c)
                .contentForEnum(c)
                .contentForConstructors(c)
                .contentForTypesInfo(c)
            }
        contentBuilder.contentForDocumentable(c, buildBlock = buildBlock)
    }

    override def contentForMember(d: Documentable) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover){ bd => bd.cover(d.getName)() }
            .divergentGroup(
                ContentDivergentGroup.GroupID("member")
            ) { divbdr => divbdr
                .instance(Set(d.getDri), sourceSets = d.getSourceSets.asScala.toSet) { insbdr => insbdr
                    .before(){ bbdr => bbdr
                        .contentForDescription(d)
                        .contentForComments(d)
                    }
                    .divergent(kind = ContentKind.Symbol) { dbdr => dbdr
                        .signature(d)
                    }
                }
            }
        contentBuilder.contentForDocumentable(d, buildBlock = buildBlock)
    }

    override def contentForFunction(f: DFunction) = contentForMember(f)

    extension (b: ScalaPageContentBuilder#ScalaDocumentableContentBuilder):
        def descriptionIfNotEmpty(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = {
            val desc = contentForDescription(d).asScala.toSeq
            val res = if desc.isEmpty then b else b
                .sourceSetDependentHint(
                    Set(d.getDri), 
                    d.getSourceSets.asScala.toSet, 
                    kind = ContentKind.SourceSetDependentHint, 
                    styles = Set(TextStyle.UnderCoverText)
                ) { sourceSetBuilder => sourceSetBuilder
                        .addChildren(desc)
                }
            res
        }

        def contentForComments(d: Documentable) = b

        def contentForDescription(d: Documentable) = {
            val specialTags = Set[Class[_]](classOf[Description])

            type SourceSet = DokkaConfiguration$DokkaSourceSet

            val tags: List[(SourceSet, TagWrapper)] =
                d.getDocumentation.asScala.toList.flatMap( (pd, doc) => doc.getChildren.asScala.map(pd -> _).toList )

            val platforms = d.getSourceSets.asScala.toSet

            val description = tags.collect{ case (pd, d: Description) => (pd, d) }.drop(1).groupBy(_(0)).map( (key, value) => key -> value.map(_(1)))

            /** Collect the key-value pairs from `iter` into a `Map` with a `cleanup` step,
              * keeping the original order of the pairs.
              */
            def collectInMap[K, E, V](
                iter: Iterator[(K, E)]
            )(
                cleanup: List[E] => V
            ): collection.Map[K, V] = {
                val lhm = mutable.LinkedHashMap.empty[K, ListBuffer[E]]
                iter.foreach { case (k, e) =>
                    lhm.updateWith(k) {
                        case None => Some(ListBuffer.empty.append(e))
                        case Some(buf) =>
                            buf.append(e)
                            Some(buf)
                    }
                }
                lhm.iterator.map { case (key, buf) => key -> cleanup(buf.result)}.to(mutable.LinkedHashMap)
            }

            val unnamedTags: collection.Map[(SourceSet, Class[_]), List[TagWrapper]] =
                collectInMap {
                    tags.iterator
                        .filterNot { t =>
                            t(1).isInstanceOf[NamedTagWrapper] || specialTags.contains(t(1).getClass)
                        }.map { t =>
                            (t(0), t(1).getClass) -> t(1)
                        }
                }(cleanup = identity)

            val namedTags: collection.Map[
                String,
                Either[
                    collection.Map[SourceSet, NamedTagWrapper],
                    collection.Map[(SourceSet, String), ScalaTagWrapper.NestedNamedTag],
                ],
            ] = {
                val grouped = collectInMap {
                    tags.iterator.collect {
                        case (sourcesets, n: NamedTagWrapper) =>
                            (n.getName, n.isInstanceOf[ScalaTagWrapper.NestedNamedTag]) -> (sourcesets, n)
                    }
                }(cleanup = identity)

                grouped.iterator.map {
                    case ((name, true), values) =>
                        val groupedValues =
                            values.iterator.map {
                                case (sourcesets, t) =>
                                    val tag = t.asInstanceOf[ScalaTagWrapper.NestedNamedTag]
                                    (sourcesets, tag.subname) -> tag
                            }.to(mutable.LinkedHashMap)
                        name -> Right(groupedValues)
                    case ((name, false), values) =>
                        name -> Left(values.to(mutable.LinkedHashMap))
                }.to(mutable.LinkedHashMap)
            }

            b.group(Set(d.getDri), styles = Set(TextStyle.Block, TableStyle.Borderless)) { bdr =>
                val b1 = description.foldLeft(bdr){
                    case (bdr, (key, value)) => bdr
                            .group(sourceSets = Set(key)){ gbdr =>
                                value.foldLeft(gbdr) { (gbdr, tag) => gbdr
                                    .comment(tag.getRoot)
                                }
                            }
                }

                b1.table(kind = ContentKind.Comment, styles = Set(TableStyle.DescriptionList)){ tbdr =>
                    val withUnnamedTags = unnamedTags.foldLeft(tbdr){ case (bdr, (key, value) ) => bdr
                        .cell(sourceSets = Set(key(0))){ b => b
                            .text(key(1).getSimpleName, styles = Set(TextStyle.Bold))
                        }
                        .cell(sourceSets = Set(key(0))) { b => b
                            .list(value, separator = ""){ (bdr, elem) => bdr
                                .comment(elem.getRoot)
                            }
                        }
                    }

                    val withNamedTags = namedTags.foldLeft(withUnnamedTags){
                        case (bdr, (key, Left(value))) =>
                            value.foldLeft(bdr){ case (bdr, (sourceSets, v)) => bdr
                                .cell(sourceSets = Set(sourceSets)){ b => b
                                    .text(key)
                                }
                                .cell(sourceSets = Set(sourceSets)){ b => b
                                    .comment(v.getRoot)
                                }
                            }
                        case (bdr, (key, Right(groupedValues))) => bdr
                            .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                .text(key)
                            }
                            .cell(sourceSets = d.getSourceSets.asScala.toSet)(_.table(kind = ContentKind.Comment, styles = Set(TableStyle.NestedDescriptionList)){ tbdr =>
                                groupedValues.foldLeft(tbdr){ case (bdr, ((sourceSets, _), v)) => bdr
                                    .cell(sourceSets = Set(sourceSets)){ b => b
                                        .comment(v.identTag)
                                    }
                                    .cell(sourceSets = Set(sourceSets)){ b => b
                                        .comment(v.descTag)
                                    }
                                }
                            })
                    }

                    val withCompanion = d match {
                        case d: DClass =>
                            val ext = d.get(ClasslikeExtension)
                            val co = ext.companion
                            co.fold(withNamedTags) { co => withNamedTags
                                .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                    .text("Companion")
                                }
                                .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                    .driLink(
                                        d.kind match {
                                            case Kind.Object => "class"
                                            case _ => "object"
                                        },
                                        co
                                    )
                                }
                            }
                        case _ => withNamedTags
                    }

                    d match{
                        case d: (WithExpectActual & WithExtraProperties[Documentable]) if d.get(SourceLinks) != null && !d.get(SourceLinks).links.isEmpty => d.get(SourceLinks).links.foldLeft(withCompanion){
                            case (bdr, (sourceSet, link)) => bdr
                                    .cell(sourceSets = Set(sourceSet)){ b => b
                                        .text("Source")
                                    }
                                    .cell(sourceSets = Set(sourceSet)){ b => b
                                        .resolvedLink("(source)", link)
                                    }
                        }
                        case other => withCompanion
                    }
                }
            }
        }

        def contentForScope(s: Documentable & WithScope) = 
            val (typeDefs, valDefs) = s.getProperties.asScala.toList.partition(_.kind == Kind.Type)
            val classes = s.getClasslikes.asScala.toList
            val inheritedDefinitions = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).inherited)
                case _ => None
            }
            val givens = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).givens)
                case p: DPackage => Option(p.get(PackageExtension)).map(_.givens)
                case _ => None
            }
            val extensions = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).extensions)
                case p: DPackage => Option(p.get(PackageExtension)).map(_.extensions)
                case _ => None
            }
            val implicitMembers = s match {
                case c: DClass => Some(c.get(ImplicitMembers))
                case _ => None
            }

            val inheritedExtensions = implicitMembers.fold(Map.empty)(_.inheritedExtensions)
            val implicitMethods = implicitMembers.fold(Map.empty)(_.methods)
            val implicitInheritedMethods = implicitMembers.fold(Map.empty)(_.inheritedMethods)
            val implicitFields = implicitMembers.fold(Map.empty)(_.properties)
            val implicitInheritedFields = implicitMembers.fold(Map.empty)(_.inheritedProperties)
            
            b
                .contentForComments(s)
                .documentableTab("Type members")(
                    DocumentableGroup(Some("Types"), typeDefs),
                    DocumentableGroup(Some("Classlikes"), classes),
                    DocumentableGroup(Some("Inherited types"), inheritedDefinitions.fold(List.empty)(_.types)),
                    DocumentableGroup(Some("Inherited classlikes"), inheritedDefinitions.fold(List.empty)(_.classlikes))
                )
                .documentableTab("Methods")(
                    DocumentableGroup(
                        Some("Defined methods"), 
                        s.getFunctions.asScala.toList
                    ),
                    DocumentableGroup(
                        Some("Inherited methods"), 
                        inheritedDefinitions.fold(List.empty)(_.methods) 
                    ),
                    DocumentableGroup(
                        Some("Implicitly Added Methods"), 
                        implicitMethods.keys.toList ++ inheritedExtensions.keys.toList ++ implicitInheritedMethods.keys.toList
                    )
                )
                .documentableTab("Value members")(
                    DocumentableGroup(
                        Some("Defined value members"), 
                        (valDefs ++ implicitFields.keys.toList)
                    ), 
                    DocumentableGroup(
                        Some("Inherited value members"), 
                        (inheritedDefinitions.fold(List.empty)(_.fields) ++ implicitInheritedFields.keys.toList)
                    )
                )
                .documentableTab("Givens")(
                     DocumentableGroup(
                        Some("Defined givens"), // TODO add extension groups!
                        givens.getOrElse(List.empty)
                    ), 
                    DocumentableGroup(
                        Some("Inherited givens"), 
                        inheritedDefinitions.fold(List.empty)(_.givens)
                    )
                )
                .documentableTab("Extensions")(
                     DocumentableGroup(
                        Some("Defined extensions"), // TODO add extension groups!
                        extensions.getOrElse(List.empty).sortBy(_.extensions.size)
                    ), 
                    DocumentableGroup(
                        Some("Inherited extensions"), 
                        inheritedDefinitions.fold(List.empty)(_.extensions).sortBy(_.extensions.size),
                    )
                )
    

        def contentForEnum(c: DClass) = 
            b.documentableTab("Enum entries")(
                DocumentableGroup(None, Option(c.get(EnumExtension)).fold(List.empty)(_.enumEntries.sortBy(_.getName).toList)), 
            )
        

        def contentForConstructors(c: DClass) = 
             b.documentableTab("Constructors")(
                DocumentableGroup(None, c.getConstructors.asScala.toList)
            )
        

        def contentForTypesInfo(c: DClass) = {
            val inheritanceInfo = c.get(InheritanceInfo)
            val supertypes = inheritanceInfo.parents
            val subtypes = inheritanceInfo.knownChildren
            def contentForType(
                bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
                b: DRI
            ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr
                .driLink(b.getClassNames, b)

            def contentForBound(
                bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
                b: Bound
            ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = b match {
                    case t: org.jetbrains.dokka.model.TypeConstructor => t.getProjections.asScala.foldLeft(bdr){
                        case (builder, p) => p match {
                            case text: UnresolvedBound => builder.text(text.getName)
                            case link: TypeParameter => builder.driLink(link.getName, link.getDri) 
                            case other => builder.text(s"TODO: $other")
                        }
                    }
                    case o => bdr.text(s"TODO: $o")
            }
            val withSupertypes = if(!supertypes.isEmpty) {
                b.header(2, "Linear supertypes")()
                .group(
                    kind = ContentKind.Comment,
                    styles = Set(ContentStyle.WithExtraAttributes), 
                    extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Linear supertypes")
                ){ gbdr => gbdr
                    .group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)){ grbdr => grbdr
                        .list(supertypes, separator = ""){ (bdr, elem) => bdr
                            .group(styles = Set(TextStyle.Paragraph))(contentForBound(_, elem))
                        }
                    }
                }
            } else b

            if(!subtypes.isEmpty) {
                withSupertypes.header(2, "Known subtypes")()
                .group(
                    kind = ContentKind.Comment,
                    styles = Set(ContentStyle.WithExtraAttributes), 
                    extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Known subtypes")
                ){ gbdr => gbdr
                    .group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)){ grbdr => grbdr
                        .list(subtypes)(contentForType)
                    }
                }
            } else withSupertypes

        }

}
