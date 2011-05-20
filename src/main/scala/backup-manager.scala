
package filebox

import java.io.{File, FileInputStream, InputStream}
import scala.collection.mutable.ListBuffer


/**
 * A class that orchestrates the backup operations. It walks through a set of
 * roots, enumerates files and sends them to the block storage collecting
 * metadata along the way and storing it too.
 *
 * Please note that the current implementation would not store all file
 * metadata, in particular the ownership will not be preserved, nor will it
 * preserve links. This works fine for the moment as the tool was intended for
 * personal backups and works well for documents.
 */
class BackupManager(
    storage: BlockStorage,
    fileCache: FileCache,
    progressReporter: BackupProgressReporter)
{

    // Default block size used for splitting the file into blocks.
    val blockSize = 16 * 1024 * 1024


    def backup(name: String, roots: List[Root]) {
        val rms: List[BackupRootManifest] =
            for {
                root <- roots
                files = backupRoot(root)
            } yield new BackupRootManifest(root.name, files)

        val m = new BackupManifest(rms)
        val mData = m.toString.getBytes("UTF-8")

        // TODO it is just bad that this write might fail if the medatada
        // record with the same name already exists, should check it first
        // probably and fail fast.
        storage.writeMetadata(name, mData, mData.size)
    }


    /**
     * Backs up a single root and returns a list of signatures of files under
     * that root. All the files are already commited to the block storage when
     * this function returns.
     */
    def backupRoot(root: Root): List[Sig] = {
        progressReporter.startRoot(root.path)

        val files = root files
        val fileSigs = new ListBuffer[Sig]()

        files foreach {f =>
            val sig = backupFile(root.path, f)
            fileSigs += sig
        }

        progressReporter.endRoot

        fileSigs.toList
    }


    /**
     * Saves the given file and returns a signature of the manifest of the file
     * in the block storage.
     */
    def backupFile(rootPath: String, path: String): Sig = {
        val file = new File(rootPath, path)

        fileCache wasModified(file) match {
            case FileWasModified() =>
                val sig = writeFile(rootPath, path, file)
                fileCache.update(file, sig)
                sig

            case FileWasNotModified(sig) =>
                progressReporter.fileSkipped(path)
                sig
        }
    }


    private def writeFile(rootPath: String, path: String, file: File): Sig = {
        val is = new FileInputStream(file)
        var blocks = new ListBuffer[Sig]()

        progressReporter.startFile(path)

        try {
            val buf = new Array[Byte](blockSize)
            var len = 0

            do {
                len = is.read(buf)
                if(len > 0) {
                    val s = storage.write(buf, len, BlockType.blockType)
                    blocks += s
                    progressReporter.blockSaved
                }
            } while(len > 0)
        } finally {
            is.close()
        }

        val st = PosixOps.stat(file.getAbsolutePath)

        // Note that the file migh have been empty and so the blocks list might
        // be empty too, this is fine, we still need to record the presence of
        // the file.
        val manifest = new FileManifest(path, st.mode, st.mtime, blocks.toList)
        val manifestData = manifest.toString.getBytes("UTF-8")
        val sig = storage.write(
            manifestData, manifestData.size, BlockType.fileType)

        progressReporter.endFile

        sig
    }
}

