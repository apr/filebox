
package filebox

import java.io.{File, FileOutputStream, IOException}


class RestoreManager(
    storage: BlockStorage,
    progressReporter: RestoreProgressReporter)
{

    /**
     * Restores a backup with the given name into the given local directory.
     */
    def restore(name: String, dir: File) {
        if(!dir.exists) {
            throw new IOException("Directory " + dir + " does not exist.")
        }

        val mData = storage.readMetadata(name)
        val manifest = BackupManifest.fromString(new String(mData, "UTF-8"))

        // Zip roots with a stream producing integers to number the roots, this
        // will help to create unique directory names.
        manifest.roots zip (Stream.from(1)) foreach {x =>
            val rm: BackupRootManifest = x._1
            val files: List[Sig] = rm.files
            val rootDir = new File(dir, rm.name getOrElse {"root" + x._2})
            restoreRoot(files, rootDir)
        }
    }


    def restoreRoot(files: List[Sig], rootDir: File) {
        rootDir.mkdirs
        files foreach {s => restoreFile(s, rootDir)}
    }


    def restoreFile(sig: Sig, rootDir: File) {
        val mData = storage.read(sig, BlockType.fileType)
        val manifest = FileManifest.fromString(new String(mData, "UTF-8"))
        val file = new File(rootDir, manifest.path)

        if(file.exists) {
            throw new IOException("Trying to restore the file " + file +
                " but it already exists")
        }

        progressReporter.startFile(manifest.path)

        // Create the file making sure that the whole directory path exists
        // first.
        file.getParentFile.mkdirs
        file.createNewFile

        val os = new FileOutputStream(file)

        try {
            for(b <- manifest.blocks) {
                val data = storage.read(b, BlockType.blockType)
                os.write(data)
                progressReporter.blockRestored
            }
        } finally {
            os.close
        }

        // TODO this works for files, but not for directories created by the
        // restore manager. The directory info is not even stored in the
        // backup!
        PosixOps.chmod(file.getAbsolutePath, manifest.mode)
        PosixOps.utimes(file.getAbsolutePath,
            Array(manifest.mtime, 0), Array(manifest.mtime, 0))

        progressReporter.endFile
    }
}

