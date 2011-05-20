
package filebox


class ConsoleBackupProgressReporter extends BackupProgressReporter {
    override def startRoot(path: String) {
        println("Backing up " + path)
    }

    override def startFile(name: String) {
        print("Saving " + name)
    }

    override def blockSaved {
        print(".")
    }

    override def endFile {
        println
    }

    override def fileSkipped(path: String) {
        println(path + " ...skipped")
    }
}


class ConsoleRestoreProgressReporter extends RestoreProgressReporter {
    override def startFile(path: String) {
        print("Restoring " + path)
    }

    override def blockRestored {
        print(".")
    }

    override def endFile {
        println
    }
}

