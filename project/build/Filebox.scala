
import com.github.retronym.OneJarProject
import sbt._


class FileboxProject(info: ProjectInfo)
    extends DefaultProject(info) with OneJarProject
{
    val scalatest = "org.scalatest" % "scalatest" % "1.3"

    override def compileOptions = Unchecked :: super.compileOptions.toList

    override def mainClass = Some("filebox.Filebox")
}

