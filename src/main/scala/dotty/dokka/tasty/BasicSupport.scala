package dotty.dokka.tasty

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import dotty.dokka._

import scala.tasty.Reflection

trait BasicSupport:
  self: TastyParser =>
  import reflect._

  object SymOps extends SymOps[reflect.type](reflect)
  export SymOps._

  extension (sym: reflect.Symbol):
    def documentation(using cxt: reflect.Context) = sym.comment match 
        case Some(comment) => 
            Map(sourceSet.getSourceSet -> parseComment(comment, sym.tree))
        case None =>  
            Map.empty

    def source(using ctx: Context) =
      val path = Some(sym.pos.sourceFile.jpath).filter(_ != null).map(_.toAbsolutePath).map(_.toString)
      path match{
        case Some(p) => Map(sourceSet.getSourceSet -> TastyDocumentableSource(p, sym.pos.startLine))
        case None => Map.empty
      }   
  
  private val emptyDRI =  DRI.Companion.getTopLevel

class SymOps[R <: Reflection](val r: R) {
  import r._

  given R = r

  extension (sym: r.Symbol):
    def packageName(using ctx: Context): String =
      if (sym.isPackageDef) sym.fullName
      else sym.maybeOwner.packageName

    def topLevelEntryName(using ctx: Context): Option[String] = if (sym.isPackageDef) None else
      if (sym.owner.isPackageDef) Some(sym.name) else sym.owner.topLevelEntryName

    def getVisibility(): ScalaVisibility =
      val visibilityFlags = (sym.flags.is(Flags.Private), sym.flags.is(Flags.Protected), sym.flags.is(Flags.Local))
      (sym.privateWithin, sym.protectedWithin, visibilityFlags) match
        case (Some(x), None, _) => ScalaVisibility.Private(Some(VisibilityScope(x.typeSymbol.name)))
        case (None, Some(x), _) => ScalaVisibility.Protected(Some(VisibilityScope(x.typeSymbol.name)))
        case (None, None, (true, false, _)) => ScalaVisibility.Private(None)
        case (None, None, (false, true, true)) => ScalaVisibility.Protected(Some(VisibilityScope("this")))
        case (None, None, (false, true, false)) => ScalaVisibility.Protected(None)
        case (None, None, (false, false, false)) => ScalaVisibility.NoModifier
        case _ => throw new Exception(s"Visibility for symbol $sym cannot be determined")

    def getModifier(): ScalaModifier =
      if (sym.flags.is(Flags.Abstract)) ScalaModifier.Abstract
      else if (sym.flags.is(Flags.Final)) ScalaModifier.Final
      else ScalaModifier.Empty

    // TODO: #49 Remove it after TASTY-Reflect release with published flag Extension
    def hackIsOpen: Boolean = {
      import dotty.tools.dotc
      given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
      val symbol = sym.asInstanceOf[dotc.core.Symbols.Symbol]
      symbol.is(dotc.core.Flags.Open)
    }

    def getExtraModifiers(): Set[ScalaOnlyModifiers] =
      Set(
        Option.when(sym.flags.is(Flags.Sealed))(ScalaOnlyModifiers.Sealed),
        Option.when(sym.flags.is(Flags.Erased))(ScalaOnlyModifiers.Erased),
        Option.when(sym.flags.is(Flags.Implicit))(ScalaOnlyModifiers.Implicit),
        Option.when(sym.flags.is(Flags.Inline))(ScalaOnlyModifiers.Inline),
        Option.when(sym.flags.is(Flags.Lazy))(ScalaOnlyModifiers.Lazy),
        Option.when(sym.flags.is(Flags.Override))(ScalaOnlyModifiers.Override),
        Option.when(sym.flags.is(Flags.Case))(ScalaOnlyModifiers.Case),
        Option.when(sym.hackIsOpen)(ScalaOnlyModifiers.Open)
      ).flatten

    def isHiddenByVisibility: Boolean = getVisibility() match
      case ScalaVisibility.Private(_) | ScalaVisibility.Protected(Some(_)) => true
      case _ => false

    def shouldDocumentClasslike: Boolean = !isHiddenByVisibility
        && !sym.flags.is(Flags.Synthetic) 
        && (!sym.flags.is(Flags.Case) || !sym.flags.is(Flags.Enum))
        && !(sym.companionModule.flags.is(Flags.Given))


    def getCompanionSymbol: Option[Symbol] = Some(sym.companionClass).filter(_.exists)

    def isCompanionObject(): Boolean = sym.flags.is(Flags.Object) && sym.companionClass.exists

    def isGiven(): Boolean = sym.flags.is(Flags.Given)

    // TODO #22 make sure that DRIs are unique plus probably reuse semantic db code?
    def dri: DRI =
      if sym == Symbol.noSymbol then emptyDRI else if sym.isValDef && sym.moduleClass.exists then sym.moduleClass.dri else
        val pointsTo =
          if (!sym.isTypeDef) PointingToDeclaration.INSTANCE
          else PointingToGenericParameters(sym.owner.typeMembers.indexOf(sym))

        val method =
          if (sym.isDefDef) Some(sym)
          else if (sym.maybeOwner.isDefDef) Some(sym.owner)
          else None

        new DRI(
          sym.packageName,
          sym.topLevelEntryName.orNull, // TODO do we need any of this fields?
          method.map(s => new org.jetbrains.dokka.links.Callable(s.name, null, Nil.asJava)).orNull,
          pointsTo, // TODO different targets?
          s"${sym.show}/${sym.signature.resultSig}/[${sym.signature.paramSigs.mkString("/")}]"
        )

  private val emptyDRI =  DRI.Companion.getTopLevel
}
