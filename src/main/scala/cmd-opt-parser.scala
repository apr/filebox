
package filebox


class CmdOptionsParsingExcepton(msg: String) extends Exception(msg)


sealed abstract class CmdCommand
case class BackupCommand(destination: String) extends CmdCommand
case class ListCommand(destination: String) extends CmdCommand
case class RestoreCommand(
    destination: String, name: String, dir: String) extends CmdCommand


class CmdOptions(
    val command: CmdCommand,
    val options: CmdOptions.OptionsMap)
{
}


/**
 * A parser for command line options.
 */
object CmdOptions {
    type OptionsMap = Map[String, String]

    def apply(opts: Array[String]): CmdOptions = {
        val cmd = parse(new CmdOptions(null, Map()), opts.toList)

        if(cmd.command == null) {
            throw new CmdOptionsParsingExcepton("Command is required")
        }

        cmd
    }


    private def parse(res: CmdOptions, opts: List[String]): CmdOptions = {
        def isOption(x: String) = x.startsWith("--")

        opts match {
            case x :: tail =>
                val (newRes, rest) =
                    if(isOption(x)) nextOption(res, opts)
                    else nextCommand(res, opts)
                parse(newRes, rest)

            case Nil => res
        }
    }


    private def nextCommand(res: CmdOptions, opts: List[String]):
        (CmdOptions, List[String]) =
    {
        if(res.command != null) {
            throw new CmdOptionsParsingExcepton("Only one command is allowed")
        }

        opts match {
            case "backup" :: dest :: tail =>
                val newRes = new CmdOptions(
                    BackupCommand(dest), res.options)
                (newRes, tail)

            case "restore" :: dest :: name :: dir :: tail =>
                val newRes = new CmdOptions(
                    RestoreCommand(dest, name, dir), res.options)
                (newRes, tail)

            case "list" :: dest :: tail =>
                val newRes = new CmdOptions(
                    ListCommand(dest), res.options)
                (newRes, tail)

            case s :: tail =>
                throw new CmdOptionsParsingExcepton(
                    "Unknown command or wrong arguments for the command: " + s)

            case Nil => (res, Nil)
        }
    }


    private def nextOption(res: CmdOptions, opts: List[String]):
        (CmdOptions, List[String]) =
    {
        opts match {
            case "--config" :: path :: tail =>
                val newRes = new CmdOptions(
                    res.command,
                    res.options ++ Map("config" -> path))
                (newRes, tail)

            case s :: tail =>
                throw new CmdOptionsParsingExcepton("Unknown option: " + s)

            case Nil => (res, Nil)
        }
    }
}

