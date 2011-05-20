
package filebox

import java.security.MessageDigest
import java.util.Arrays


/**
 * A file that represents a message digest.
 */
class Sig(val digest: Array[Byte]) {

    override def equals(rh: Any): Boolean = {
        rh match {
            case o: Sig => Arrays.equals(this.digest, o.digest)
            case _ => false
        }
    }

    override def hashCode: Int = Arrays.hashCode(digest)

    override def toString: String = {
        val sb = new StringBuilder

        for(b <- digest) {
            sb.append(String.format("%02x", new java.lang.Integer(0xff & b)))
        }

        sb.toString
    }
}


object Sig {
    def sha256(buf: Array[Byte], len: Int): Sig = {
        val md = MessageDigest.getInstance("SHA-256")
        md.update(buf, 0, len)
        new Sig(md.digest)
    }

    def apply(buf: Array[Byte], len: Int) = sha256(buf, len)

    /**
     * Convert a string representation of the digest into an instance of the
     * Sig class. The string should be in hex form.
     */
    def fromString(str: String): Sig = {
        if(str.length != 64) {
            throw new IllegalArgumentException("Incorrectly formed " +
                "signature string: '" + str + "'")
        }

        val sig = new Array[Byte](32)
        for(b <- 0 to 31) {
            val i = b * 2
            val v = java.lang.Integer.parseInt(
                str.substring(i, i + 2), 16).toByte
            sig(b) = v
        }

        return new Sig(sig)
    }
}

