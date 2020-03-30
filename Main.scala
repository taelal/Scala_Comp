import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val file = new File(lib)
    val files = file.listFiles()

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {
        for (line <- source.getLines())
        {
          cmd = line.split(" ").head
          cmd match {

            //Memory access cases
            case "push" =>
            case "pop" =>

            //arithmetics cases
            case "add" =>
            case "sub" =>

            //boolean cases
            case "eq" =>
            case "neq" =>
            case "gt" =>
            case "lt" =>
            case "and" =>
            case "or" =>
            case "not" =>
          }
        }
        val writer = new PrintWriter(new FileOutputStream(f,true))
        writer.close()
      }
    }
  }
  def writeToFile(myTxt: String, filePath: String): Unit = {
    val files = file.listFiles()
    for (f <- files) {
        val writer = new PrintWriter(new FileOutputStream(f, true))
        writer.close()
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
  def add (lib: String) : Unit = String{
  }

  def sub (lib: String) : Unit = String{
  }

  def neg (lib: String) : Unit = String{
  }

  def eq (lib: String) : Unit = String{
  }

  def gt (lib: String) : Unit = String{
  }

  def lt (lib: String) : Unit = String{
  }

  def and (lib: String) : Unit = String{
  }

  def or (lib: String) : Unit = String {
  }

  def not (lib: String) : Unit = String{
  }

  //Memory access commands

  def push (segment: String, index: String) : Unit = {

    var translateSeg : String = ""
    segment match {
      //Memory access cases
      case "constant" =>
        translateSeg = "@" + index +
          "\nD=A" +
          "\n@SP" +
          "\nA=M" + // next available place in the stack
          "\nM=D" + // push index into the stack
          "\n@SP" +
          "\nM=M+1\n" // move the stack pointer to the next available place
      case "local" => //push into the top of the stack the value that is in address RAM[ RAM [LCL] + x ]
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@LCL" +
          "\nA=M+D" + //a = ram[lcl] + index
          "\nD=M" +  // d = ram[ram[lcl] + index]
          "\n@SP" +
          "\nM=D"+
          "\n@SP" +
          "\nM=M+1\n"
      case "argument" =>
      case "this" =>
      case "that" =>
      case "temp" =>
      case "static" =>
    }
    /*@2
    D=A
    @SP
    A=M
    M=D
    @SP
    M=M+1*/
  }

  def pop (segment: String, index: String) : Unit = String {
  }
}

