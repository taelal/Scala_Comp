import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {

  var curFile : String = ""

  def main(args: Array[String]): Unit = {
    val file = new File(lib)
    val files = file.listFiles()

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {

        curFile = f.getName() //className

        for (line <- curFile.getLines())
        {
          var cmd = line.split(" ").head
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
    /*val files = file.listFiles()
    for (f <- files) {
        val writer = new PrintWriter(new FileOutputStream(f, true))
        writer.close()
    }*/
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
  def add (lib: String) :  String = {
    return ""
  }

  def sub (lib: String) :  String = {
    return ""
  }

  def neg (lib: String) : String = {
    return ""
  }

  def eq (lib: String) : String = {
    return ""
  }



  def gt (lib: String) : String = {
    return ""
  }

  def lt (lib: String) : String = {
    return ""
  }

  def and (lib: String) : String = {
    return ""
  }

  def or (lib: String) : String = {
    return ""
  }

  def not (lib: String) : String = {
    return  ""
  }

  //Memory access commands

  def push (segment: String, index: String) :  String = {
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
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@ARG" +
          "\nA=M+D" + //a = ram[lcl] + index
          "\nD=M" +  // d = ram[ram[lcl] + index]
          "\n@SP" +
          "\nM=D"+
          "\n@SP" +
          "\nM=M+1\n"
      case "this" =>
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@THIS" +
          "\nA=M+D" + //a = ram[lcl] + index
          "\nD=M" +  // d = ram[ram[lcl] + index]
          "\n@SP" +
          "\nM=D"+
          "\n@SP" +
          "\nM=M+1\n"
      case "that" =>
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@THAT" +
          "\nA=M+D" +
          "\nD=M" +
          "\n@SP" +
          "\nM=D"+
          "\n@SP" +
          "\nM=M+1\n"
      case "temp" =>
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@5" +
          "\nD=A+D" + //D = 5 + index
          "\n@D" +
          "\nD=M" +  // d = ram[5 + index]
          "\n@SP" +
          "\nM=D" +
          "\n@SP" +
          "\nM=M+1\n"
      case "static" =>
        translateSeg = "@" + curFile + "." + index +
          "\nD=M" + // d = RAM[curFile.index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D" +
          "\n@SP" +
          "\nM=M+1\n"
    }
    return translateSeg
  }

  /*@INDEX
    D=A
    @LCL
    A=M+D //A=RAM[LCL]+INDEX
    D=M //D=RAM[LCL] + INDEX
    A=D
    D=M //RAM[RAM[LCL] + INDEX]
    @SP
    A=M-1 // A = RAM[SP]-1
    D=M //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
    @SP
    M=M-1
   */






  def pop (segment: String, index: String) :  String = {
    var translateSeg : String = ""
    segment match {
      case "local" => //push into the top of the stack the value that is in address RAM[ RAM [LCL] + x ]
        translateSeg = "@" + index +
          "\nD=A" + // d = index
          "\n@LCL" +
          "\nA=M+D" + //a = ram[lcl] + index
          "\nD=M" + // d = ram[ram[lcl] + index]
          "\nA=D" +
          "\nD=M" + //RAM[RAM[LCL] + INDEX]
          "\n@SP" +
          "\nA=M-1" + // A = RAM[SP]-1
          "\nM=D"+ ////RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP" +
          "\nM=M-1\n"
      case "argument" =>

      case "this" =>

      case "that" =>

      case "temp" =>

      case "static" =>
      return translateSeg
    }
  }
}

