import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {

  var curFile : String = ""

  def main(args: Array[String]): Unit = {
    val file = new File("C:\\Users\\tahel\\Documents\\AAA_Study\\Ecronot\\Levels\\ex_nurit\\Exercises\\Exercises\\Targil1\\project_07\\MemoryAccess\\BasicTest")
    val files = file.listFiles()
    var translationLine : String = " "

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {
        var asmfname : String = f.getName().replace(".vm",".asm")

        val writer = new PrintWriter(new FileOutputStream(asmfname,true))

        curFile = f.getName() //className

        for (line <- Source.fromFile(curFile).getLines())
        {
          var curWordList = line.split(" ")
          var cmd = curWordList(0)

          cmd match {

            //Memory access cases
            case "push" => translationLine = push( curWordList(1), curWordList(2))
            case "pop" => translationLine = pop( curWordList(1), curWordList(2))

            //arithmetics cases
            case "add" => translationLine = add(cmd)
            case "sub" => translationLine = sub(cmd)

            //boolean cases
            case "eq" => translationLine = eq(cmd)
            case "neg" => translationLine = neg(cmd)
            case "gt" => translationLine = gt(cmd)
            case "lt" => translationLine = lt(cmd)
            case "and" => translationLine = and(cmd)
            case "or" => translationLine = or(cmd)
            case "not" => translationLine = not(cmd)
            case default =>
          }
          writer.write(translationLine)
        }
        writer.close()
      }
    }
  }
  /*
  /*
        "\nM=M-1" + //A=RAM[SP]-1
        "\n@SP"  + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
        "\nA=M" + //A=RAM[SP]-2 --M=SECOND_POP
        "\nD=M" + //M=RAM[RAM[SP]-1] + RAM[RAM[SP]-2]
        "\n@SP"   +
        "\nM=M-1" +
        "\n@SP" +
        "\nA=M" +
        "\nA=M" +
        "\nD=D+A" +
        "\n@SP" +
        "\nA=M" +
        "\nM=D"
   */
   */
  //functions to convert from vm to asm
  def add (lib: String) :  String = {
    //pop pop compute push
    var translateSeg = "@SP" +
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
    var translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=M-D" + //M=RAM[RAM[SP]-1] + RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def neg (lib: String) : String = {
    //pop compute push
    var translateSeg = "@SP" +
      "\nA=M-1" +
      "\n@SP"   +
      "\nD=M" +
      "\nD=M" +
      "\nM=-D\n"
    return translateSeg
  }

  /*
  d = first_pop - second_pop
  if d == 0 push -1
  else: push 0
*/
  def eq (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "@SP" +
      "\nA=M-1" + //RAM[SP] = RAM[SP] - 1 --RAM[SP]=POINTER TO TOP VAL IN THE STACK
      "\nD=M"   +
      "\nA=A-1" +
      "\nD=D-M" + //D = RAM[RAM[SP]] -- D=TOP VAL IN THE STACK --FIRST_POP
      "\n@IF_TRUE0" +
      "\nD; JEQ" +
      "\nD=0" +
      "\n@SP" + //A=POINTER TO NEXT TOP VAL IN THE STACK
      "\nA=M-1" + //RAM[SP]=NEXT TOP VAL IN THE STACK --SECOND_POP
      "\nA=A-1" +
      "\nM=D" + //A = LABLE TRUE
      "\n@IF_FALSE0 " + //IF D==0 --TRUE
      "\n0; JMP" + //A = LABLE FALSE
      "\n(IF_TRUE0)" + //JUMP ANYWAY
      "\nD=-1" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" + //POINTER TO TOP OF STACK = RAM[SP]
      "\n(IF_FALSE0)" + //RAM[TOP_OF_STACK] = 0
      "\n@SP" +
      "\nM=M-1\n" //MOVING SP TO NEXT AVAILABLE PLACE
    return translateSeg
  }

  def gt (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "@SP" +
      "\nA=M-1" + //RAM[SP] = RAM[SP] - 1 --RAM[SP]=POINTER TO TOP VAL IN THE STACK
      "\nD=M"   +
      "\nA=A-1" +
      "\nD=D-M" + //D = RAM[RAM[SP]] -- D=TOP VAL IN THE STACK --FIRST_POP
      "\n@IF_TRUE0" +
      "\nD; JLQ" +
      "\nD=0" +
      "\n@SP" + //A=POINTER TO NEXT TOP VAL IN THE STACK
      "\nA=M-1" + //RAM[SP]=NEXT TOP VAL IN THE STACK --SECOND_POP
      "\nA=A-1" +
      "\nM=D" + //A = LABLE TRUE
      "\n@IF_FALSE0 " + //IF D==0 --TRUE
      "\n0; JMP" + //A = LABLE FALSE
      "\n(IF_TRUE0)" + //JUMP ANYWAY
      "\nD=-1" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" + //POINTER TO TOP OF STACK = RAM[SP]
      "\n(IF_FALSE0)" + //RAM[TOP_OF_STACK] = 0
      "\n@SP" +
      "\nM=M-1\n" //MOVING SP TO NEXT AVAILABLE PLACE
    return translateSeg
  }

  def lt (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "@SP" +
      "\nA=M-1" + //RAM[SP] = RAM[SP] - 1 --RAM[SP]=POINTER TO TOP VAL IN THE STACK
      "\nD=M"   +
      "\nA=A-1" +
      "\nD=D-M" + //D = RAM[RAM[SP]] -- D=TOP VAL IN THE STACK --FIRST_POP
      "\n@IF_TRUE0" +
      "\nD; JGT" +
      "\nD=0" +
      "\n@SP" + //A=POINTER TO NEXT TOP VAL IN THE STACK
      "\nA=M-1" + //RAM[SP]=NEXT TOP VAL IN THE STACK --SECOND_POP
      "\nA=A-1" +
      "\nM=D" + //A = LABLE TRUE
      "\n@IF_FALSE0" + //IF D<0 --TRUE
      "\n0; JMP" + //A = LABLE FALSE
      "\n(IF_TRUE0)" + //JUMP ANYWAY
      "\nD=-1" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" + //POINTER TO TOP OF STACK = RAM[SP]
      "\n(IF_FALSE0)" + //RAM[TOP_OF_STACK] = 0
      "\n@SP" +
      "\nM=M-1\n" //MOVING SP TO NEXT AVAILABLE PLACE
    return translateSeg
  }

  def and (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "@SP" +
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
    var translateSeg = "@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D|M" + //M=RAM[RAM[SP]-1] | RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def not (lib: String) : String = {
    var translateSeg = "@SP" +
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
      case "pointer" =>
        translateSeg = ""
        if(index == 0)
        {
          translateSeg += "@THIS\n"
        }
        else
        {
          translateSeg += "@THAT\n"
        }
        translateSeg += "\nD=M" +
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
          "\nD=A"+ //D=INDEX
          "\n@LCL"+
          "\nA=M+D"+ //A=RAM[LCL]+INDEX
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
          "\nD=M"+ //RAM[RAM[LCL] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "that" =>
        translateSeg = "@" + index +
          "\nD=A"+ //D=INDEX
          "\n@THAT"+
          "\nA=M+D"+ //A=RAM[THAT]+D
          "\nD=M"+ //RAM[RAM[THAT] + INDEX]
          "\n@SP"+
          "\nA=M-1"+ //A = RAM[SP] - 1
          "\nD=M "+ //RAM[RAM[THAT] + INDEX]] = RAM[RAM[SP]-1]
          "\n@SP" +
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
        translateSeg = "@SP" +
          "\nM=M-1" +
          "\n@SP" +
          "\nA=M" +
          "\nD=M" +
          "\n@" + curFile + "." + index +
          "\nM=D\n"
      case "pointer" =>
        var pointerNum : String = ""
         if (index == 0)
           {
             pointerNum = "@THIS"
           }
        else
         {
           pointerNum = "\n@THAT"
         }
        translateSeg = "\n@SP" +
          "\nA=M-1" +
          "\nD=M" +
          pointerNum +
          "\nM=D" +
          "\n@SP" +
          "\nM=M-1\n"
    }
    return translateSeg
  }
}