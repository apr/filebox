
package filebox.spec

import org.scalatest.FunSuite


class SpecParserTest extends FunSuite {

    test("remove quotes from string") {
        def sq = parser_util.stripQuotes _
        assert(sq("") === "")
        assert(sq("\"t\"") === "t")
        assert(sq("test") === "test")
    }

    test("parse empty string") {
        val p = new SpecParser
        val s = p.parseString("")
        assert(s.elements.size === 0)
    }

    test("parse int value") {
        val p = new SpecParser
        val s = p.parseString("i = 10")
        assert(s.elements.size === 1)
        assert(s.elements(0) === IntValue("i", 10))
    }

    test("parse string value") {
        val p = new SpecParser
        val s = p.parseString("s = \"test\"")
        assert(s.elements.size === 1)
        assert(s.elements(0) === StringValue("s", "test"))
    }

    test("parse string with quotes") {
        val p = new SpecParser
        val s = p.parseString("""s = "t\"" """)
        assert(s.elements.size === 1)
        assert(s.elements(0) === StringValue("s", "t\""))
    }

    test("parse string array") {
        val p = new SpecParser
        val s = p.parseString("a = [\"v1\", \"v2\"]")
        assert(s.elements.size === 1)
        assert(s.elements(0) === StringArray("a", List("v1", "v2")))
    }

    test("parse multiple value pairs") {
        val p = new SpecParser
        val s = p.parseString("i1 = 1\ni2 = 2")
        assert(s.elements.size === 2)
        assert(s.elements(0) === IntValue("i1", 1))
        assert(s.elements(1) === IntValue("i2", 2))
    }

    test("parse single group") {
        val p = new SpecParser
        val s = p.parseString("g { i = 1\n}")
        assert(s.elements.size === 1)
        assert(s.elements(0) === SpecGroup("g", List(IntValue("i", 1))))
    }

    test("parse multiple groups") {
        val p = new SpecParser
        val s = p.parseString("g1{i = 1}g2{i = 1}")
        assert(s.elements.size === 2)
        assert(s.elements(0) === SpecGroup("g1", List(IntValue("i", 1))))
        assert(s.elements(1) === SpecGroup("g2", List(IntValue("i", 1))))
    }

    test("parse a value and a group") {
        val p = new SpecParser
        val s = p.parseString("i = 1\n\ng {\n s = \"v\"\n}")
        assert(s.elements.size === 2)
        assert(s.elements(0) === IntValue("i", 1))
        assert(s.elements(1) === SpecGroup("g", List(StringValue("s", "v"))))
    }

    test("parse a subgroup") {
        val p = new SpecParser
        val s = p.parseString("g { gg {}}")
        assert(s.elements.size === 1)
        assert(s.elements(0) === SpecGroup("g", List(SpecGroup("gg", Nil))))
    }

    test("parse a comment") {
        val p = new SpecParser
        val s = p.parseString("# comment\n")
        assert(s === new Spec(Nil))
    }

    test("parse a block comment") {
        val p = new SpecParser
        val s = p.parseString("# line1\n# line2")
        assert(s === new Spec(Nil))
    }

    test("parse comments") {
        val p = new SpecParser
        val s = p.parseString("# c1\n# bc1\ni = 1 # c2\ng { # c3\n}\n  # c4")
        assert(s.elements.size === 2)
        assert(s.elements(0) === IntValue("i", 1))
        assert(s.elements(1) === SpecGroup("g", Nil))
    }
}

