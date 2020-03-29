import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {

  def writeToFile(lib: String): Unit = {
    val file = new File(lib)
    val files = file.listFiles()
    for (f <- files) {
      if (f.getName().endsWith(".vm")) {
        val writer = new PrintWriter(new FileOutputStream(f, true))
        writer.close()
      }
    }
  }

  def readFromFile(): Unit = {
    val source = Source.fromFile("hello.vm")
    val writer = new PrintWriter(new File("hello.asm"))
    for (line <- source.getLines()) {
      if (line.startsWith("you") || line.startsWith("are")) {
        println(line)
      }
      writer.write(line + "\n")
    }
    source.close()
    writer.close()
  }

  //functions to convert from vm to asm
  def add(): Unit = {
  }

  def sub () : Unit = {
  }

  def neg () : Unit = {
  }

  def eq () : Unit = {
  }

  def gt () : Unit = {
  }

  def lt () : Unit = {
  }

  def and () : Unit = {
  }

  def or () : Unit = {
  }

  def not () : Unit = {
  }

  //Memory access commands
  def push () : Unit = {
  }

  def pop () : Unit = {
  }
}

