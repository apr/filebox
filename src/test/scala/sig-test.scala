
package filebox

import org.scalatest.FunSuite


class SigTest extends FunSuite {
    test("well known sha256 string") {
        val sig = Sig("a".getBytes, 1)
        val s =
            "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
        assert(s === sig.toString)
    }

    test("sing equality") {
        val b1: Array[Byte] = "a".getBytes
        val b2: Array[Byte] = "b".getBytes
        assert(Sig(b1, 1) === Sig(b1, 1))
        assert(Sig(b1, 1) != Sig(b2, 1))
    }

    test("from string") {
        val s =
            "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
        assert(Sig("a" getBytes, 1) === Sig.fromString(s))
    }
}

