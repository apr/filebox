
package filebox.spec

import annotation._


/**
 * Converts a spec structure into a string.
 */
class SpecFormatter {
    val defaultIndent = 4

    // Main method, converts the given spec structure into a string.
    def format(spec: Spec): String = {
        val b = new StringBuilder
        for(e <- spec.elements) b append formatElement(e) append "\n"
        b.toString
    }

    def mkIndent(size: Int): StringBuilder = {
        val ret = new StringBuilder
        Range(0, size) foreach (_ => ret.append(' '))
        ret
    }

    def formatElement(e: SpecElement, indent: Int = 0): String = {
        e match {
            case i: IntValue => formatInt(i, indent)
            case s: StringValue => formatString(s, indent)
            case sa: StringArray => formatStringArray(sa, indent)
            case g: SpecGroup => formatGroup(g, indent)
        }
    }

    def formatInt(i: IntValue, indent: Int = 0): String = {
        val b = mkIndent(indent)
        b append i.key append " = " append i.value
        b.toString
    }

    def formatString(s: StringValue, indent: Int = 0): String = {
        val b = mkIndent(indent)
        b append s.key append " = \"" append escape(s.value) append "\""
        b.toString
    }

    def formatStringArray(a: StringArray, indent: Int = 0): String = {
        @tailrec
        def formatVals(
            b: StringBuilder,
            xs: List[String],
            sep: String,
            indent: Int)
        {
            xs match {
                case Nil =>
                case x::xss =>
                    if(!sep.isEmpty) b append sep append "\n"
                    b append mkIndent(indent)
                    b append '"' append escape(x) append '"'
                    formatVals(b, xss, ",", indent)
            }
        }

        val b = mkIndent(indent)
        b append a.key append " = [\n"

        val bl = b.length
        formatVals(b, a.value, "", indent + defaultIndent)

        if(b.length > bl) {
            b append "\n"
        }

        b append mkIndent(indent) append "]"
        b.toString
    }

    def formatGroup(g: SpecGroup, indent: Int = 0): String = {
        val b = mkIndent(indent)

        b append g.name append " {\n"

        for(e <- g.elements) {
            b append formatElement(e, indent + defaultIndent) append "\n"
        }

        b append mkIndent(indent) append "}"
        b.toString
    }

    /**
     * Escapes certain characters in the given string.
     */
    def escape(str: String) = {
        (str map {x =>
            x match {
                case 0x0d => "\\r"
                case 0x0a => "\\n"
                case 0x09 => "\\t"
                case 0x22 => "\\\""
                case 0x5c => "\\\\"
                case c => c
            }
        }).mkString("") 
    }
}

