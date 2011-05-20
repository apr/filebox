
package filebox.spec


sealed abstract class SpecElement

sealed abstract class SpecValue(val key: String) extends SpecElement

case class IntValue(
    override val key: String,
    value: Int) extends SpecValue(key)

case class StringValue(
    override val key: String,
    value: String) extends SpecValue(key)

case class StringArray(
    override val key: String,
    value: List[String]) extends SpecValue(key)

case class SpecGroup(
        name: String,
        elements: List[SpecElement]) extends SpecElement {

    validateValueNames(elements)

    
    def array(key: String): Option[List[String]] = elements collect {
        case StringArray(k, v) if k == key => v
    } headOption
        
    def intValue(key: String) = elements collect {
        case IntValue(k, v) if k == key => v
    } headOption

    def stringValue(key: String) = elements collect {
        case StringValue(k, v) if k == key => v
    } headOption

    def groups(name: String): List[SpecGroup] = elements collect {
        case g @ SpecGroup(n, _) if n == name => g
    }

    // Throws IllegalArgumentException if there are multiple values (not
    // groups!) with the same name or the name is empty. Returns uneventfully
    // otherwise.
    private def validateValueNames(elements: List[SpecElement]) {
        def elementName(e: SpecElement) = {
            e match {
                case t: SpecValue => Some(t.key)
                case _ => None
            }
        }

        def checkName(n: String, seenNames: Set[String]) {
            if(n isEmpty) {
                throw new IllegalArgumentException("Empty value name")
            }

            if(seenNames contains n) {
                throw new IllegalArgumentException("Duplicate names: " + n)
            }
        }

        def validateValueNames2(
                elements: List[SpecElement],
                seenNames: Set[String]) {
            elements match {
                case x::xs =>
                    elementName(x) match {
                        case Some(n) =>
                            checkName(n, seenNames)
                            validateValueNames2(xs, seenNames + n)
                        case None =>
                    }
                case _ =>
            }
        }

        validateValueNames2(elements, Set.empty)
    }
}


class Spec(override val elements: List[SpecElement])
    extends SpecGroup("", elements)
{
}


object Spec {
    def apply(elems: List[SpecElement]) = new Spec(elems)
}

