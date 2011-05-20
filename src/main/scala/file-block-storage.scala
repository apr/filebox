
package filebox

import java.io.{File, FileInputStream, FileOutputStream, IOException}
import spec._


/**
 * An implementation of the block storage that stores data on the local
 * filesystem. The path to the directory where to store the data is passed to
 * the constructor. IOException will be thrown if the path is not a directory
 * of if the directory can not be created.
 */
class FileBlockStorage(val path: String) extends BlockStorage {

    private val dataDir = new File(path, "blocks")
    private val metaDir = new File(path, "meta")

    checkOrCreateDir(dataDir)
    checkOrCreateDir(metaDir)


    override def write(data: Array[Byte], len: Int, dataType: String): Sig = {
        val sig = Sig(data, len)
        val fname = sig.toString + "." + dataType 
        val dir = blockDir(fname)
        val file = new File(dir, fname)

        // If the file already exists, do nothing, we rely on the hash to
        // ensure that the contents is the same.
        if(!file.exists) {
            dir.mkdirs

            // First write to a temp file.
            val tmpFile = File.createTempFile("block-tmp", null,
                file.getParentFile)
            tmpFile.deleteOnExit

            val os = new FileOutputStream(tmpFile)

            try {
                os.write(data, 0, len)
            } finally {
                os.close
            }

            tmpFile.renameTo(file)
        }

        sig
    }


    override def read(digest: Sig, dataType: String): Array[Byte] = {
        val fname = digest.toString + "." + dataType 
        val dir = blockDir(fname)
        val file = new File(dir, fname)

        if(!file.exists) {
            throw new IOException("Block " + digest + " of type " + dataType +
                " does not exist")
        }

        val ret = new Array[Byte](file.length.toInt)
        val is = new FileInputStream(file)

        try {
            is.read(ret)
        } finally {
            is.close
        }

        ret
    }


    override def listMetadata: List[String] = metaDir.list.toList


    override def writeMetadata(name: String, data: Array[Byte], len: Int) {
        val file = new File(metaDir, name)

        if(file.exists) {
            throw new IOException(name + " already exists")
        }

        // First write to a temp file.
        val tmpFile = File.createTempFile("meta-tmp", null,
            file.getParentFile)
        tmpFile.deleteOnExit

        val os = new FileOutputStream(tmpFile)

        try {
            os.write(data, 0, len)
        } finally {
            os.close
        }

        tmpFile.renameTo(file)
    }



    override def readMetadata(name: String): Array[Byte] = {
        val file = new File(metaDir, name)

        if(!file.exists) {
            throw new IOException("There is no metadata record " + name)
        }

        val ret = new Array[Byte](file.length.toInt)
        val is = new FileInputStream(file)

        try {
            is.read(ret)
        } finally {
            is.close
        }

        ret
    }


    private def checkOrCreateDir(dir: File) {
        if(dir.exists() && !dir.isDirectory()) {
            throw new IOException(dir + " exists but is not a directory")
        } else if(!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Could not create the directory " + dir)
        }
    }


    private def blockDir(fname: String): File =
        new File(dataDir, fname.substring(0, 2))
}


object FileBlockStorage {
    /**
     * Creates FileBlockStorage from its definition as a spec group.
     */
    def apply(definition: SpecGroup): FileBlockStorage = {
        val path = definition.stringValue("path") getOrElse {
            val name = definition.stringValue("name").get
            throw new IllegalArgumentException("Destination " + name +
                " has no path")
        }

        new FileBlockStorage(path)
    }
}

