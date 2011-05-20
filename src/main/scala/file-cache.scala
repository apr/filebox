
package filebox

import java.io.File


/**
 * Result of the file cache lookup.
 */
sealed abstract class FileCacheResult
case class FileWasModified() extends FileCacheResult
case class FileWasNotModified(sig: Sig) extends FileCacheResult


/**
 * An interface for a cache that stores file modification time. This cache is
 * useful to determine whether a local file needs backup or not based on its
 * modification time since last store.
 */
trait FileCache {

    /**
     * Returns whether the given file was modified or not since the last
     * update() call. It will also return FileWasModified if the file is not
     * known to the cache. If the file was not modified then its signature will
     * be in the result.
     */
    def wasModified(file: File): FileCacheResult

    /**
     * Update the modification time of the given file in the cache.
     */
    def update(file: File, sig: Sig)
}

