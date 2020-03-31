import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {

  var curFile : String = ""

  def main(args: Array[String]): Unit = {
    val file = new File(lib)
    val files = file.listFiles()
    var translationLine : String = ""

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {
        val writer = new PrintWriter(new FileOutputStream(f,true))
        curFile = f.getName() //className

        for (line <- curFile.getLines())
        {
          var curWordList = line.split(" ")
          var cmd = curWordList[0]
          cmd match {

            //Memory access cases
            case "push" => translationLine = push(curWordList[1],curWordList[2])
            case "pop" => translationLine = pop(curWordList[1],curWordList[2])

            //arithmetics cases
            case "add" => translationLine = add(cmd)
            case "sub" => translationLine = sub(cmd)

            //boolean cases
            case "eq" => translationLine = eq(cmd)
            case "neq" => translationLine = neq(cmd)
            case "gt" => translationLine = gt(cmd)
            case "lt" => translationLine = lt(cmd)
            case "and" => translationLine = and(cmd)
            case "or" => translationLine = or(cmd)
            case "not" => translationLine = not(cmd)
          }
          writer.write(translationLine)
        }
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
      writer.write(line)
    }
    source.close()
    writer.close()
  }

  //functions to convert from vm to asm
  def add (lib: String) :  String = {
    //pop pop compute push
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D+M" + //M=RAM[RAM[SP]-1] + RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def sub (lib: String) :  String = {
    //pop pop compute push
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=M-D" + //M=RAM[RAM[SP]-1] + RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def neg (lib: String) : String = {
    return ""
  }

  /*
  d = x-y
  a = y-x
  push !(a|d)
   */
  def eq (lib: String) : String = {
    //pop pop compute push
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D&M" + //M=RAM[RAM[SP]-1] & RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def gt (lib: String) : String = {
    return ""
  }

  def lt (lib: String) : String = {
    return ""
  }

  def and (lib: String) : String = {
    //pop pop compute push
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D&M" + //M=RAM[RAM[SP]-1] & RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def or (lib: String) : String = {
    //pop pop compute push
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D|M" + //M=RAM[RAM[SP]-1] | RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def not (lib: String) : String = {
    translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nM=!M\n" //RAM[RAM[SP]-1]=!RAM[RAM[SP]-1]
    return translateSeg
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

  def pop (segment: String, index: String) :  String = {
    var translateSeg : String = ""
    segment match {
      case "local" => //push into the top of the stack the value that is in address RAM[ RAM [LCL] + x ]
        translateSeg = "@" + index +
          "\nD=A"+
          "\n@LCL"+
          "\nA=M+D"+
          "\nA=M"+ //A=RAM[LCL] + INDEX
          "\nD=M"+ //RAM[RAM[LCL] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "argument" =>
        translateSeg = "@" + index +
          "\nD=A"+
          "\n@ARG"+
          "\nA=M+D"+
          "\nA=M"+ //A=RAM[LCL] + INDEX
          "\nD=M"+ //RAM[RAM[LCL] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "this" =>
        translateSeg = "@" + index +
          "\nD=A"+
          "\n@THIS"+
          "\nA=M+D"+
          "\nA=M"+ //A=RAM[LCL] + INDEX
          "\nD=M"+ //RAM[RAM[LCL] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "that" =>
        translateSeg = "@" + index +
          "\nD=A"+
          "\n@THAT"+
          "\nA=M+D"+
          "\nA=M"+ //A=RAM[LCL] + INDEX
          "\nD=M"+ //RAM[RAM[LCL] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "temp" =>
        translateSeg = "@" + index +
          "\nD=A"+ //D = INDEX
          "\n@5"+
          "\nA=A+D"+ //A=5 + INDEX
          "\nD=M"+ //D=RAM[5 + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M"+ //RAM[5 + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "static" =>
        translateSeg = "@" + curFile + "." + index +
          "\nD=M" + // d = RAM[curFile.index]
          "\n@SP" +
          "\nA=A-1" + //A=SP-1
          "\nD=M" + //RAM[curFile.index] = RAM[SP-1]
          "\n@SP" +
          "\nM=M-1\n"
    }
    return translateSeg
  }
}