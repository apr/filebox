
package filebox.spec

import org.scalatest.FunSuite


class SpecFormatterTest extends FunSuite {

    test("mkIndent") {
        val f = new SpecFormatter
        assert(f.mkIndent(2).toString() === "  ")
    }

    test("format int value") {
        val f = new SpecFormatter
        assert(f.formatInt(IntValue("i", 1)) === "i = 1")
    }

    test("format string value") {
        val f = new SpecFormatter
        val s = f.formatString(StringValue("s", "test"))
        assert(s === "s = \"test\"")
    }

    test("format string array") {
        val f = new SpecFormatter
        assert(f.formatStringArray(StringArray("a", Nil)) ===
            "a = [\n]")
        assert(f.formatStringArray(StringArray("a", List("v1"))) ===
            "a = [\n    \"v1\"\n]")
        assert(f.formatStringArray(StringArray("a", List("v1", "v2"))) ===
            "a = [\n    \"v1\",\n    \"v2\"\n]")
    }

    test("format group") {
        val f = new SpecFormatter
        assert(f.formatGroup(SpecGroup("g", Nil)) ===
            "g {\n}")
        assert(f.formatGroup(SpecGroup("g", List(IntValue("i", 1)))) ===
            "g {\n    i = 1\n}")
        assert(f.formatGroup(SpecGroup("g", List(SpecGroup("gg", Nil)))) ===
            "g {\n    gg {\n    }\n}")
    }

    test("escape string") {
        val f = new SpecFormatter
        val s = f.escape("\t\n\r\"")
        assert(s === "\\t\\n\\r\\\"")
    }
}

