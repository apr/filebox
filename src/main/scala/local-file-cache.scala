
package filebox

import java.io._
import java.util.zip._


/**
 * An implementation of a file cache that uses a local file to record
 * modification times.
 */
class LocalFileCache(cacheFile: File) extends FileCache {
    // A map from <file path> to <last modification time>.
    type CacheMap = Map[String, (Long, Sig)]

    private var cache = readCache


    def wasModified(file: File): FileCacheResult = {
        cache.get(file.getAbsolutePath) match {
            case Some((time, sig)) => if(file.lastModified > time)
                FileWasModified() else FileWasNotModified(sig)
            case None => FileWasModified()
        }
    }


    def update(file: File, sig: Sig) {
        cache = cache ++ Map(file.getAbsolutePath -> (file.lastModified, sig))
    }


    /**
     * Totally blows out the cache in memory and on disk.
     */
    def clearCache {
        cache = Map()
        saveCache
    }


    private def readCache: CacheMap = {
        def readFromReader(reader: BufferedReader, acc: CacheMap): CacheMap = {
            val line = reader.readLine
            if(line != null) {
                val sp1 = line.indexOf(' ')
                val sp2 = line.indexOf(' ', sp1 + 1)
                val time = java.lang.Long.parseLong(line.substring(0, sp1))
                val sig = Sig.fromString(line.substring(sp1 + 1, sp2))
                val path = line.substring(sp2 + 1)
                readFromReader(reader, acc ++ Map(path -> (time, sig)))
            } else {
                acc
            }
        }

        if(!cacheFile.exists) {
            return Map()
        }

        val fis = new FileInputStream(cacheFile)
        // If the file is not gzipped it is assumed to be plain text.
        val zis = try { new GZIPInputStream(fis) } catch { case _ => fis }
        val reader = new BufferedReader(new InputStreamReader(zis, "UTF-8"))

        try {
            readFromReader(reader, Map())
        } finally {
            reader.close; zis.close; fis.close
        }
    }


    /**
     * Persists the cache to the cache file.
     */
    def saveCache {
        // First write the cache to the temporary file and then rename it to
        // ensure integrity of data on disk.
        val tmpFile = File.createTempFile(
            "cache-tmp", null, cacheFile.getParentFile)
        tmpFile.deleteOnExit

        val fos = new FileOutputStream(tmpFile)
        val zos = new GZIPOutputStream(fos)
        val writer = new BufferedWriter(new OutputStreamWriter(zos, "UTF-8"))

        try {
            for((path, (time, sig)) <- cache) {
                writer.write(time.toString)
                writer.write(' ')
                writer.write(sig.toString)
                writer.write(' ')
                writer.write(path)
                writer.newLine
            }
        } finally {
            writer.close; zos.close; fos.close
        }

        tmpFile.renameTo(cacheFile)
    }
}

