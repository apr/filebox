
package filebox

import scala.Console._


object ConsoleUtil {
    def cursorLeft(n: Int) {
        print("\033[" + n + "D")
    }

    def cursorUp(n: Int) {
        print("\033[" + n + "A")
    }
}


class ConsoleBackupProgressReporter extends BackupProgressReporter {
    override def startRoot(path: String) {
        println("Backing up " + path)
    }

    override def startFile(name: String) {
        print(GREEN + "Saving " + RESET + name)
    }

    override def blockSaved {
        print(BLUE + "." + RESET)
    }

    override def endFile {
        println
    }

    override def fileSkipped(path: String) {
        println(path + GREEN + " ...skipped" + RESET)
    }
}


class ConsoleRestoreProgressReporter extends RestoreProgressReporter {
    override def startFile(path: String) {
        print(GREEN + "Restoring " + RESET + path)
    }

    override def blockRestored {
        print(BLUE + "." + RESET)
    }

    override def endFile {
        println
    }
}

