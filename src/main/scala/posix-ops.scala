
package filebox

import java.io.File
import org.jruby.ext.posix._
import scala.collection.JavaConversions._


object PosixOps {
    // Just an alias so that the users of this objet wouldn't have to import
    // the jruby package.
    type FileStat = org.jruby.ext.posix.FileStat

    class Handler extends POSIXHandler {
        import POSIXHandler.WARNING_ID

        def error(error: POSIX.ERRORS, extraData: String) {
            throw new Exception("Posix error: " + error.toString())
        }

        def unimplementedError(methodName: String) {
            throw new UnsupportedOperationException(methodName +
                " is not supported in PosixOps")
        }

        def warn(id: WARNING_ID, message: String, data: AnyRef*) {
        }

        def isVerbose: Boolean = false

        def getCurrentWorkingDirectory: File =
            new File(System.getProperty("user.dir"))

        def getEnv: Array[String] =
            (System.getenv map {x => x._1 + "=" + x._2}).toArray

        def getPID: Int = 0

        def getInputStream = System.in
        def getOutputStream = System.out
        def getErrorStream = System.err
    }

    val instance = POSIXFactory.getPOSIX(new Handler, true)


    // Posix method delegates.

    def chmod(path: String, mode: Int) = instance.chmod(path, mode)
    def stat(path: String) = instance.stat(path)

    /**
     * The time is specified as an array of two values - sec and usec. The time
     * arrays should always have two elements. They can be null though if
     * current time is desired.
     */
    def utimes(path: String, atimeval: Array[Long], mtimeval: Array[Long]) =
        instance.utimes(path, atimeval, mtimeval)
}

