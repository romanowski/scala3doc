package tests
package visibility

private object PrivateTopLevelObject //unexpected

private[tests] object PrivateInOuterPackageTopLevelObject //unexpected

private[visibility] object PrivateInInnerPackageTopLevelObject //unexpected

private[this] object LocallyPrivateTopLevelObject //unexpected

protected object ProtectedTopLevelObject

protected[tests] object ProtectedInOuterPackageTopLevelObject //unexpected

protected[visibility] object ProtectedInInnerPackageTopLevelObject //unexpected

protected[this] object LocallyProtectedTopLevelObject //unexpected

private def PrivateTopLevelMethod: Int //unexpected
    = 1

protected def ProtectedTopLevelMethod: Int
    = 1

class InClassVisibility()
{
    private def privateMethod: Int //unexpected
      = ???

    private[tests] def privateInOuterPackageMethod: Int //unexpected
      = ???

    private[visibility] def privateInInnerPackageMethod: Int //unexpected
      = ???

    private[InClassVisibility] def privateInClassMethod: Int //unexpected
      = ???

    private[this] def locallyPrivateMethod: Int //unexpected
      = ???

    protected def protectedMethod: Int
      = ???

    protected[tests] def protectedInOuterPackageMethod: Int //unexpected
      = ???

    protected[visibility] def protectedInInnerPackageMethod: Int //unexpected
      = ???

    protected[InClassVisibility] def protectedInClassClassMethod: Int //unexpected
      = ???
    
    protected[this] def locallyProtectedMethod: Int //unexpected
      = ???
}
