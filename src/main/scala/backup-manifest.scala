
package filebox

import spec._


/**
 * A class that describes a single root in the backup.
 */
class BackupRootManifest(
    val name: Option[String],
    val files: List[Sig])
{
    def toSpecGroup = {
        val elems = List(StringArray("files", files map {_.toString}))
        SpecGroup("root",
            if(name.isDefined) StringValue("name", name.get) :: elems
            else elems)
    }
}


/**
 * A class that represents metadata for the whole backup.
 */
class BackupManifest(val roots: List[BackupRootManifest]) {
    
    def toSpec: Spec = Spec(roots map {_.toSpecGroup})

    override def toString: String = {
        val f = new SpecFormatter
        f.format(toSpec)
    }
}


object BackupManifest {
    def fromSpec(spec: Spec): BackupManifest = {
        val roots =
            for {
                g <- spec.groups("root")
                name = g.stringValue("name")
                fnames = g.array("files") getOrElse List()
                fs = fnames map {n => Sig.fromString(n)}
            } yield new BackupRootManifest(name, fs)

        new BackupManifest(roots)
    }

    def fromString(str: String): BackupManifest = {
        val p = new SpecParser
        fromSpec(p.parseString(str))
    }
}

