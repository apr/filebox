
package filebox

import spec._


/**
 * A description of a file in the block storage.
 */
class FileManifest(
    val path: String,
    val mode: Int,
    val mtime: Long,
    val blocks: List[Sig])
{

    def toSpec: Spec = {
        val elems = List(
            StringValue("path", path),
            IntValue("mode", mode),
            IntValue("mtime", mtime.toInt),  // TODO fix this, should be long
            StringArray("blocks", blocks map {_.toString}))
        new Spec(elems)
    }

    override def toString: String = {
        val f = new SpecFormatter
        f.format(toSpec)
    }

    override def equals(rh: Any): Boolean = {
        rh match {
            case o: FileManifest =>
                path == o.path && blocks == o.blocks
            case _ => false
        }
    }

    override def hashCode: Int = path.hashCode + blocks.hashCode
}


object FileManifest {
    def fromSpec(spec: Spec) = {
        val path = spec.stringValue("path") getOrElse
            {throw new IllegalArgumentException("No path in the spec.")}
        val mode = spec.intValue("mode") getOrElse
            {throw new IllegalArgumentException("No mode in the spec.")}
        val mtime = spec.intValue("mtime") getOrElse
            {throw new IllegalArgumentException("No mtime in the spec.")}
        val blocks = spec.array("blocks") getOrElse
            {throw new IllegalArgumentException("No blocks in the spec.")}

        new FileManifest(
            path, mode, mtime, blocks map {x => Sig.fromString(x)})
    }

    def fromString(str: String) = {
        val p = new SpecParser
        fromSpec(p.parseString(str))
    }
}

