
package filebox

import org.scalatest.FunSuite
import spec._


class FileManifestTest extends FunSuite {
    test("conversion to spec") {
        val s1 = Sig("a".getBytes, 1)
        val s2 = Sig("b".getBytes, 1)
        val m1 = new FileManifest("/test", 1, 2, Nil)
        val m2 = new FileManifest("/test", 3, 4, List(s1, s2))

        val expected1 = Spec(List(
            StringValue("path", "/test"),
            IntValue("mode", 1),
            IntValue("mtime", 2),
            StringArray("blocks", Nil)))
        val expected2 = Spec(List(
            StringValue("path", "/test"),
            IntValue("mode", 3),
            IntValue("mtime", 4),
            StringArray("blocks", List(s1.toString, s2.toString))))

        assert(m1.toSpec === expected1)
        assert(m2.toSpec === expected2)
    }

    test("equals") {
        val s1 = Sig("a".getBytes, 1)
        val s2 = Sig("b".getBytes, 1)
        val m1 = new FileManifest("/test", 1, 2, List(s1))
        val m2 = new FileManifest("/test", 1, 2, List(s1))
        val m3 = new FileManifest("/test2", 1, 2, List(s2))
        assert(m1 === m2)
        assert(m1 != m3)
        assert(m1.hashCode === m2.hashCode)
    }

    test("read from spec, incomplete data") {
        intercept[IllegalArgumentException] {
            FileManifest.fromSpec(Spec(Nil))
        }

        // Missing blocks.
        intercept[IllegalArgumentException] {
            FileManifest.fromSpec(Spec(List(StringValue("path", "/"))))
        }

        // Missing path.
        intercept[IllegalArgumentException] {
            FileManifest.fromSpec(Spec(List(StringArray("blocks", Nil))))
        }
    }

    test("read from spec") {
        val sig = Sig("a".getBytes, 1)
        val spec = Spec(
            List(StringValue("path", "/test"),
            IntValue("mode", 1),
            IntValue("mtime", 2),
            StringArray("blocks", List(sig.toString))))
        val m = FileManifest.fromSpec(spec)
        assert(m === new FileManifest("/test", 1, 2, List(sig)))
    }
}

