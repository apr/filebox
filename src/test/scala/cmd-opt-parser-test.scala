
package filebox

import org.scalatest.FunSuite


class CmdOptionsParserTest extends FunSuite {

    test("empty options") {
        // A command is required, so empty options will throw.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array())
        }
    }

    test("a single option") {
        val opts = CmdOptions(Array("--config", "path", "backup", "dest"))
        assert(opts.command === BackupCommand("dest"))
        assert(opts.options === Map("config" -> "path"))
    }

    test("backup command") {
        val opts = CmdOptions(Array("backup", "dest"))
        assert(opts.command === BackupCommand("dest"))
        assert(opts.options === Map())

        // Not enough arguments.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("backup"))
        }
    }

    test("restore command") {
        val opts = CmdOptions(Array("restore", "dest", "name", "dir"))
        assert(opts.command === RestoreCommand("dest", "name", "dir"))
        assert(opts.options === Map())

        // Not enough arguments.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("restore"))
        }

        // Not enough arguments.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("restore", "dest"))
        }

        // Not enough arguments.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("restore", "dest", "name"))
        }
    }

    test("list command") {
        val opts = CmdOptions(Array("list", "dest"))
        assert(opts.command === ListCommand("dest"))
        assert(opts.options === Map())

        // Not enough arguments.
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("list"))
        }
    }

    test("wrong command") {
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("non-existent-command"))
        }
    }

    test("wrong option") {
        intercept[CmdOptionsParsingExcepton] {
            CmdOptions(Array("--non-existent-option", "backup"))
        }
    }

    test("an option and a command") {
        val opts = CmdOptions(Array("--config", "c", "list", "dest"))
        assert(opts.command === ListCommand("dest"))
        assert(opts.options === Map("config" -> "c"))
    }
}

