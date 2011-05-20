
package filebox

import annotation._
import java.io.{File, FileNotFoundException}


/**
 * A class that represents a directory in the local filesystem that should be
 * backed up. The root may have a list of files to exclude, the exclude file
 * paths should be relative to the root's path. The excludes may list whole
 * subdirectories.
 *
 * If the directory at the given path does not exist FileNotFoundException will
 * be thrown for all operations trying to get files.
 */
class Root(
    val path: String,
    val name: Option[String],
    val excludes: Iterable[String])
{
    /**
     * Returns a list of files under the root. The file paths returned are
     * relative to the root's path and the excludes has already been handled.
     */
    def files: Seq[String] = {
        def isExcluded(p: String) = excludes exists {p.startsWith(_)}
        allFiles filterNot isExcluded
    }


    /**
     * Returns all files under the root regardless of the specified excludes.
     */
    def allFiles: Seq[String] = {
        @tailrec
        def walkDirs(fs: List[File], acc: List[File]): List[File] = {
            fs match {
                case f::fss =>
                    if(f isDirectory) walkDirs(fss ++ f.listFiles, acc)
                    else walkDirs(fss, f::acc)
                case Nil => acc
            }
        }

        def makeRelative(f: File) = f.getAbsolutePath.substring(path.length + 1)

        val p = new File(path)

        if(!p.exists) {
            throw new FileNotFoundException(path + " does not exist.")
        }

        walkDirs(List(p), Nil) map {makeRelative(_)}
    }
}

