
package filebox.spec

import org.scalatest.FunSuite


class SpecTest extends FunSuite {

    test("extract an array from a group") {
        val a1 = StringArray("a1", List("v1"))
        val a2 = StringArray("a2", List("v2"))
        val g1 = SpecGroup("g1", List(a1))
        val g2 = SpecGroup("g2", List(a1, a2))

        assert(g1.array("a1") === Some(List("v1")))
        assert(g1.array("a2") === None)
        assert(g2.array("a2") === Some(List("v2")))
    }

    test("extract an int value from a group") {
        val i1 = IntValue("i1", 1)
        val i2 = IntValue("i2", 2)
        val g1 = SpecGroup("g1", List(i1))
        val g2 = SpecGroup("g2", List(i1, i2))

        assert(g1.intValue("i1") === Some(1))
        assert(g1.intValue("i2") === None)
        assert(g2.intValue("i2") === Some(2))
    }

    test("extract a string value from a group") {
        val s1 = StringValue("s1", "str1")
        val s2 = StringValue("s2", "str2")
        val g1 = SpecGroup("g1", List(s1))
        val g2 = SpecGroup("g2", List(s1, s2))

        assert(g1.stringValue("s1") === Some("str1"))
        assert(g1.stringValue("s2") === None)
        assert(g2.stringValue("s2") === Some("str2"))
    }

    test("extract subgroups from a group") {
        val sg1 = SpecGroup("sg1", Nil)
        val sg2 = SpecGroup("sg1", Nil)
        val sg3 = SpecGroup("sg2", Nil)
        val g1 = SpecGroup("g1", List(sg1, sg2, sg3))

        assert(g1.groups("sg1") === List(sg1, sg2))
        assert(g1.groups("sg2") === List(sg3))
        assert(g1.groups("sg3") === Nil)
    }

    test("empty value names are prohibited") {
        intercept[IllegalArgumentException] {
            SpecGroup("g", List(IntValue("", 1)))
        }
    }

    test("Duplicate value names are prohibited") {
        intercept[IllegalArgumentException] {
            SpecGroup("g", List(IntValue("v1", 1), IntValue("v1", 1)))
        }
    }

    test("spec equality") {
        val s1 = new Spec(List(IntValue("i", 1)))
        val s2 = new Spec(List(IntValue("i", 1)))
        val s3 = new Spec(List(IntValue("i", 2)))
        assert(s1 === s2)
        assert(s1 != s3)
    }
}

