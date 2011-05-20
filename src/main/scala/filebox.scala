
package filebox

import java.io.File
import spec._


object Filebox {

    // Returns a File object for the default config, which is .fileboxrc in the
    // user's home directory.
    def defaultConfigFile: File = {
        val home = java.lang.System.getenv("HOME")
        new File(home, ".fileboxrc")
    }

    // Returns a File object for the directory where all caches will be stored.
    def defaultCacheDir: File = {
        val home = java.lang.System.getenv("HOME")
        new File(home, ".filebox/cache")
    }

    // Generates a simple backup name consisting of a hostname and current
    // date.
    def generateBackupName = {
        val df = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
        util.hostname + "-" + df.format(new java.util.Date)
    }

    def usage() {
        println("Usage:")
        println("filebox <options> <command> <command parameters>")
        println()
        println("where options:")
        println("\t--config <path to config file>")
        println()
        println("commands:")
        println("\tlist <destination> - list all backups")
        println("\tbackup <destination> - backup to the destination")
        println("\trestore <destination> <name> <local dir>")
    }

    def getConfigFile(opts: CmdOptions): File = {
        val cfg = for(c <- opts.options.get("config")) yield new File(c)
        cfg getOrElse defaultConfigFile
    }


    // Reads the filbox config from the given file.
    def readCondig(file: File): Spec = {
        val p = new SpecParser
        p.parseString(util.fileToString(file))
    }


    // Looks up definition for the given destination name and creates an
    // appropriate block storage.
    def storageForDestination(config: Spec, destination: String) = {
        val ds = config groups "destination"
        val dds = ds filter {x =>
            val name = x.stringValue("name").getOrElse(false)
            name == destination
        }

        if(dds.isEmpty) {
            throw new Exception("Destination " + destination +
                " is not defined")
        }

        if(dds.size > 1) {
            throw new Exception("There are multiple definitions for " +
                destination)
        }

        // Now we are sure that there is one and only one destination.
        val dest = dds.head

        dest.stringValue("type") match {
            case Some("file") => FileBlockStorage(dest)

            case Some("s3") => S3BlockStorage(dest)

            case Some(t) => throw new Exception("Unknown destination type " +
                "in the config: " + t);

            case None => throw new Exception("Destination " + destination +
                " has no type")
        }
    }


    // Get a list of roots to backup from the configuration.
    def rootsFromConfig(config: Spec): List[Root] = {
        val rs = (config groups "root") filter
            (r => r.stringValue("path").isDefined)
        rs map {r =>
            val path = r.stringValue("path").get
            val name = r.stringValue("name")
            val excludes = r.array("exclude").getOrElse(Nil)
            new Root(path, name, excludes)
        }
    }


    // TODO check if backup with the same name already exists.
    def handleBackup(config: Spec, destination: String) {
        val bs = storageForDestination(config, destination)

        val cacheDir = defaultCacheDir
        val cacheFile = new File(cacheDir, destination + ".file.cache.gz")

        cacheDir.mkdirs

        val fc = new LocalFileCache(cacheFile)
        val pr = new ConsoleBackupProgressReporter
        val bm = new BackupManager(bs, fc, pr)
        val backupName = generateBackupName

        println("Writing backup '" + backupName + "'")

        bm.backup(backupName, rootsFromConfig(config))
        fc.saveCache

        println("Done.")
    }


    def handleList(config: Spec, destination: String) {
        val s = storageForDestination(config, destination)
        s.listMetadata.foreach (println _)
    }


    def handleRestore(config: Spec,
                      destination: String,
                      name: String,
                      dir: String)
    {
        val s = storageForDestination(config, destination)
        val pr = new ConsoleRestoreProgressReporter
        val rm = new RestoreManager(s, pr)
        val d = new File(dir)

        if(d.exists) {
            throw new Exception(dir + " already exists")
        }

        println("Restoring '" + name + "'")
        d.mkdirs
        rm.restore(name, d)
        println("Done")
    }


    def silenceAwsLog {
        System.getProperties().setProperty(
            "org.apache.commons.logging.Log",
            "org.apache.commons.logging.impl.NoOpLog")
        
    }


    def main(args: Array[String]) {
        silenceAwsLog

        try {
            val opts = CmdOptions(args)
            val config = readCondig(getConfigFile(opts))

            opts.command match {
                case ListCommand(dest) => handleList(config, dest)
                case BackupCommand(dest) => handleBackup(config, dest)
                case RestoreCommand(dest, name, dir) =>
                    handleRestore(config, dest, name, dir)
            }
        } catch {
            case ex: CmdOptionsParsingExcepton =>
                println(ex.getMessage); println; usage()
                System.exit(1)

            case ex: Exception =>
                println("ERROR: " + ex.getMessage)
                System.exit(1)
        }
    }
}

