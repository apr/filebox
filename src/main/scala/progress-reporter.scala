
package filebox


/**
 * An interface that the backup manager uses to report its progress.
 */
trait BackupProgressReporter {

    def startRoot(path: String) {
    }

    def endRoot {
    }

    def startFile(path: String) {
    }

    // Called after the backup manager has successfully saved a block.
    def blockSaved {
    }

    def endFile {
    }

    // Called when the backup manager decided not to store the file most
    // likely due to the modification date being too old. This method will be
    // called insted of start/endFile.
    def fileSkipped(path: String) {
    }
}


/**
 * An interface that the restore manager uses to report its progress.
 */
trait RestoreProgressReporter {
    def startFile(path: String) {
    }

    def blockRestored {
    }

    def endFile {
    }
}

