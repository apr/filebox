
package filebox

import java.io.{File, FileInputStream, InputStreamReader}


/**
 * A collection of utility functions.
 */
object util {

    /**
     * Returns contents of the given file as a string. May throw IOException.
     */
    def fileToString(file: File): String = {
        val sb = new StringBuffer
        val buf = new Array[Char](1024)

        val is = new FileInputStream(file)
        val r = new InputStreamReader(is, "UTF-8")

        try {
            var len = 0
            while({len = r.read(buf); len != -1}) {
                sb.append(buf, 0, len)
            }
        } finally {
            r.close; is.close
        }

        sb.toString
    }


    /**
     * Returns the hostname of this machine.
     */
    def hostname = {
        val addr = java.net.InetAddress.getLocalHost
        addr.getHostName
    }
}

