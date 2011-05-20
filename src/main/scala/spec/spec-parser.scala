
package filebox.spec

import java.io.StringReader
import scala.util.parsing.combinator._


object parser_util {
    def stripQuotes(s: String) =
        if(s.length > 1 && s(0) == 0x22 && s(s.length - 1) == 0x22)
            s.substring(1, s.length - 1) else s
}


class SpecParser extends JavaTokenParsers {
    import parser_util._

    def spec: Parser[Spec] = specElements ^^ (x => new Spec(x))

    def specElement = specValue ^^ (x => Some(x)) |
                      comment ^^^ None

    def specElements = rep(specElement) ^^
        (x => x collect { case Some(x) => x })

    def specValue = group | valuePair

    // Parses a comment which starts with a '#' and continues to the end of the
    // line.
    def comment = """\#.*\n""".r | """\#.*\z""".r

    def group: Parser[SpecGroup] = ident ~ "{" ~ specElements ~ "}" ^^
        { case i ~ "{" ~ x ~ "}" => SpecGroup(i, x) }

    def valuePair: Parser[SpecValue] = intValue | stringValue | stringArray

    def intValue: Parser[IntValue] = ident ~ "=" ~ decimalNumber ^^
        { case i ~ "=" ~ v => IntValue(i, v.toInt) }

    def stringValue: Parser[StringValue] = ident ~ "=" ~ string ^^
        { case s ~ "=" ~ v => StringValue(s, stripQuotes(v)) }

    def unicode = rep1("\\u" ~> """[a-fA-F0-9]{4}""".r) ^^ { stringBytes =>
        new String(stringBytes.map(
            Integer.valueOf(_, 16).intValue.asInstanceOf[Char]).toArray)
    }

    def escaped = "\\" ~> """[\\/bfnrt"]""".r ^^ { charStr =>
        val char = charStr match {
            case "r" => '\r'
            case "n" => '\n'
            case "t" => '\t'
            case "b" => '\b'
            case "f" => '\f'
            case x => x.charAt(0)
        }
        char.toString
    }

    def characters = """[^\"[\x00-\x1F]\\]+""".r

    def string: Parser[String] = "\"" ~>
        rep(unicode | escaped | characters) <~ "\"" ^^ { list =>
            list.mkString("")
        }
    

    // A helper that parses string array in the form of '["val", ...]'.
    def stringArrayValues: Parser[List[String]] = "[" ~>
        (repsep(string, ",") <~ "]")

    def stringArray: Parser[StringArray] = ident ~ "=" ~ stringArrayValues ^^
        { case i ~ "=" ~ v => StringArray(i, v map stripQuotes) }


    /**
     * Parses the given string into a spec structure.
     * TODO should throw some specific exception
     */
    def parseString(in: String) =
        parseAll(spec, new StringReader(in)) match {
            case Success(res, _) => res
            case f => throw new Exception(f.toString)
        }
}

