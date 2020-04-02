/*
  Exercise_1 By Tahel El Al (324012129) and Adva Cohen (323840561)
  From VM to ASM basic data access, statics and arithmetics.
 */

import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

object Main {

  var curFile : String = ""

  def main(args: Array[String]): Unit = {
    val file = new File("C:\\Users\\tahel\\Documents\\AAA_Study\\Ecronot\\Levels\\Exersice_1")
    val files = file.listFiles()
    var translationLine : String = " "

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {
        var asmfname : String = f.getName().replace(".vm",".asm")

        val writer = new PrintWriter(new FileOutputStream(asmfname,true))

        curFile = f.getName() //className

        for (line <- Source.fromFile(f).getLines())
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
            case default => translationLine = ""
          }
          writer.write(translationLine)
        }
        writer.close()
      }
    }
  }

  //functions to convert from vm to asm
  def add (lib: String) :  String = {
    //pop pop compute push
    var translateSeg = "\n//ADD" + "\n@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M = SECOND_POP
      "\nM=D+M" + //M=RAM[RAM[SP]-1] + RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" //move the stack pointer to the new available place
    return translateSeg
  }

  def sub (lib: String) :  String = {
    //pop pop compute push
    var translateSeg = "\n//SUB" + "\n@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D = RAM[RAM[SP]-1 ] --FIRST POP
      "\nA=A-1" + //A=RAM[SP]-2 -- RAM[A] = SECOND POP
      "\nM=M-D" + //M=RAM[RAM[SP]-2] - RAM[RAM[SP]-1]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def neg (lib: String) : String = {
    //pop compute push
    var translateSeg = "\n//NEG" + "\n@SP" +
      "\nA=M-1" +
      "\nD=M" +
      "\nM=-D\n" //push neg val of top val in stack
    return translateSeg
  }

  /*
  d = first_pop - second_pop
  if d == 0 push -1
  else: push 0
*/
  def eq (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "\n//EQ" +
      "\n@SP" +
      "\nA=M-1" +
      "\nD=M"   + //D=RAM[RAM[SP]-1] --FIRST POP
      "\nA=A-1" +  //MOVING TO THE SECOND POP
      "\nD=D-M" + //D=RAM[RAM[SP]-1] - RAM[RAM[SP]-2]
      "\n@IF_TRUE0" +
      "\nD;JEQ" + //IF D == 0 -- IF EQUAL JUMP TO TRUE
      "\nD=0" +
      "\n@SP" +
      "\nA=M-1" + //RAM[SP] = RAM[RAM[SP]-1] --MOVING SP POINTER TO TOP VAL IN STACK --POP
      "\nA=A-1" +
      "\nM=D" + // TOP VAL IN STACK = 0
      "\n@IF_FALSE0" +
      "\n0; JMP" + //JUMP ANYWAY TO FALSE
      "\n(IF_TRUE0)" +
      "\nD=-1" + //D=TRUE
      "\n@SP" +
      "\nA=M-1" + //SP = RAM[SP]-1 --MOVING SP POINTER TO NEXT TOP VAL IN STACK --POP
      "\nA=A-1" + //RAM[SP]-1 = RAM[RAM[SP]-1] - 1
      "\nM=D" + //TOP OF THE STACK = -1 --IF TRUE
      "\n(IF_FALSE0)" +
      "\n@SP" +
      "\nM=M-1\n" //MOVING SP TO NEXT AVAILABLE PLACE
    return translateSeg
  }

  def gt (lib: String) : String = {
    //pop pop compute push
    var translateSeg =  "\n//GT" + "\n@SP" +
      "\nA=M-1" +
      "\nD=M"   +
      "\nA=A-1" +
      "\nD=D-M" +
      "\n@IF_TRUE1" +
      "\nD;JLT" +
      "\nD=0" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" +
      "\n@IF_FALSE1" +
      "\n0; JMP" +
      "\n(IF_TRUE1)" +
      "\nD=-1" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" +
      "\n(IF_FALSE1)" +
      "\n@SP" +
      "\nM=M-1\n"
    return translateSeg
  }

  def lt (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "\n//LT" + "\n@SP" +
      "\nA=M-1" +
      "\nD=M"   +
      "\nA=A-1" +
      "\nD=D-M" +
      "\n@IF_TRUE2" +
      "\nD;JGT" +
      "\nD=0" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" +
      "\n@IF_FALSE2" +
      "\n0; JMP" +
      "\n(IF_TRUE2)" +
      "\nD=-1" +
      "\n@SP" +
      "\nA=M-1" +
      "\nA=A-1" +
      "\nM=D" +
      "\n(IF_FALSE2)" +
      "\n@SP" +
      "\nM=M-1\n"//MOVING SP TO NEXT AVAILABLE PLACE
    return translateSeg
  }

  def and (lib: String) : String = {
    //pop pop compute push
    var translateSeg = "\n//AND" + "\n@SP" +
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
    var translateSeg = "\n//OR" + "\n@SP" +
      "\nA=M-1" + //A=RAM[SP]-1
      "\nD=M"   + //D=RAM[RAM[SP]-1 ] (FIRST_POP)
      "\nA=A-1" + //A=RAM[SP]-2 --M=SECOND_POP
      "\nM=D|M" + //M=RAM[RAM[SP]-1] | RAM[RAM[SP]-2]
      "\n@SP"   +
      "\nM=M-1\n" // move the stack pointer to the new available place
    return translateSeg
  }

  def not (lib: String) : String = {
    var translateSeg = "\n//NOT" + "\n@SP" +
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
        translateSeg = "//PUSH CONSTANT" + "\n@" + index +
          "\nD=A" +
          "\n@SP" +
          "\nA=M" + // next available place in the stack
          "\nM=D" + // push index into the stack
          "\n@SP" +
          "\nM=M+1\n" // move the stack pointer to the next available place
      case "local" => //push into the top of the stack the value that is in address RAM[ RAM [LCL] + x ]
        translateSeg = "//PUSH LOCAL" + "\n@" + index +
          "\nD=A" + // d = index
          "\n@LCL" +
          "\nA=M+D" + //a = ram[lcl] + index
          "\nD=M" +  // d = ram[ram[lcl] + index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D"+ //ram[ram[SP]] = ram[ram[LCL] + index]
          "\n@SP" +
          "\nM=M+1\n"
      case "argument" =>
        translateSeg = "\n//PUSH ARG" +
          "\n@" + index +
          "\nD=A" + // d = index
          "\n@ARG" +
          "\nA=M+D" + //a = ram[ARG] + index
          "\nD=M" +  // d = ram[ram[ARG] + index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D"+ //ram[ram[SP]] = ram[ram[ARG] + index]
          "\n@SP" +
          "\nM=M+1\n"
      case "this" =>
        translateSeg = "\n//PUSH THIS" + "\n@" + index +
          "\nD=A" + // d = index
          "\n@THIS" +
          "\nA=M+D" + //a = ram[THIS] + index
          "\nD=M" +  // D = ram[ram[THIS] + index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D"+ //ram[ram[SP]] = ram[ram[THIS] + index]
          "\n@SP" +
          "\nM=M+1\n"
      case "that" =>
        translateSeg = "\n//PUSH THAT" + "\n@" + index +
          "\nD=A" + // d = index
          "\n@THAT" +
          "\nA=M+D" +
          "\nD=M" + // D = ram[ram[THAT] + index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D"+ //ram[ram[SP]] = ram[ram[THAT] + index]
          "\n@SP" +
          "\nM=M+1\n"
      case "temp" =>
        var plus_5 :Int = (index.toInt + 5) //5+index
        translateSeg = "\n//PUSH TEMP" +
          "\n@" +
          plus_5.toString +
          "\nD=M" + //D=RAM[5+INDEX]
          "\n@SP" +
          "\nA=M" +
          "\nM=D" + //RAM[RAM[SP]] = RAM[5+INDEX]
          "\n@SP" +
          "\nM=M+1\n"
      case "static" =>
        translateSeg = "@" + curFile + "." + index +
          "\nD=M" + // D = RAM[curFile.index]
          "\n@SP" +
          "\nA=M" +
          "\nM=D" + //RAM[RAM[SP]] = RAM[curFile.index]
          "\n@SP" +
          "\nM=M+1\n" //MOVING SP TO THE NEXT AVAILABLE PLACE
      case "pointer" =>
        translateSeg = ""
        if(index == "0")
        {
          translateSeg +=  "\n@THIS\n"
        }
        else
        {
          translateSeg += "\n@THAT\n"
        }
        translateSeg +=
          "\n//pointer push" +
          "\nD=M" + //D=RAM[POINTER]
          "\n@SP" +
          "\nA=M" +
          "\nM=D" + //RAM[SP] = RAM[POINTER]
          "\n@SP" +
          "\nM=M+1\n" //MOVING SP TO THE NEXT AVAILABLE PLACE
      case default => translateSeg = ""
    }
    return translateSeg
  }

  def pop (segment: String, index: String) :  String = {
    var translateSeg : String = ""
    var translateIndex : String = ""
    for (i <- 0 to index.toInt - 1)
    {
      translateIndex += "\nA=A+1\n"
    }
    segment match {
      case "local" => //push into the top of the stack the value that is in address RAM[ RAM [LCL] + x ]
        translateSeg = "\n//POP LCL" + "\n@" +"SP"+
          "\nA=M-1"+
          "\nD=M"+ //D=RAM[RAM[SP] - 1] -- TOP VALUE IN THE STACK
          "\n@LCL"+
          "\nA=M"+ //RAM[RAM[LCL] + INDEX]
          translateIndex +
          "\nM=D"+ //RAM[RAM[LCL] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP "+
          "\nM=M-1\n"
      case "argument" =>
        translateSeg = "\n//POP ARG" + "\n@" +"SP"+
          "\nA=M-1"+
          "\nD=M"+  //D=RAM[RAM[SP] - 1] -- TOP VALUE IN THE STACK
          "\n@ARG"+
          "\nA=M"+ //RAM[RAM[ARG] + INDEX]
          translateIndex + //REPEAT A=A+1 INDEX TIMES
          "M=D"+ //RAM[RAM[THIS] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP "+
          "\nM=M-1\n"
      case "this" =>
        translateSeg = "\n//POP THIS" +
          "\n@SP"+
          "\nA=M-1"+
          "\nD=M"+ //D=RAM[RAM[SP] - 1] -- TOP VALUE IN THE STACK
          "\n@THIS"+
          "\nA=M"+ //RAM[RAM[THIS] + INDEX]
          translateIndex + //REPEAT A=A+1 INDEX TIMES
          "M=D"+ //RAM[RAM[THIS] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP "+
          "\nM=M-1\n"
      case "that" =>
        translateSeg = "\n//POP THAT" +
          "\n@SP"+
          "\nA=M-1"+
          "\nD=M"+ //D = RAM[RAM[SP]-1]
          "\n@THAT"+
          "\nA=M"+ //A=RAM[THAT]
          translateIndex + //REPEAT A=A+1 INDEX TIMES
          "M=D"+ //RAM[RAM[THAT] + INDEX] = RAM[RAM[SP-1]]
          "\n@SP "+
          "\nM=M-1\n"
      case "temp" =>
        var plus5 :Int = (index.toInt + 5)
        translateSeg = "\n//POP TEMP" +
          "\n@SP" +
          "\nA=M-1"+
          "\nD=M"+ //D = RAM[RAM[SP] - 1] -- TOP VAL IN THE STACK
          "\n@"+ plus5.toString +
          "\nM=D"+ //RAM[5 + INDEX] = RAM[RAM[SP-1]]
          "\n@SP"+
          "\nM=M-1\n"
      case "static" =>
        translateSeg = "\n//POP STATIC" +
          "\n@SP" +
          "\nM=M-1" + // RAM[SP] = RAM[SP]-1 --MOVING POINTER SP TO CURRENT TOP VAL IN STACK
          "\n@SP" +
          "\nA=M" +
          "\nD=M" + //D=RAM[RAM[SP]]
          "\n@" + curFile + "." + index +
          "\nM=D\n" //RAM[CURFILE.INDEX] = RAM[RAM[SP]]
      case "pointer" =>
        var pointerNum : String = ""
        if (index == "0")
        {
          pointerNum = "\n//pointer pop 0" +"\n@THIS"
        }
        if (index == "1")
        {
          pointerNum = "\n//pointer pop 1" + "\n@THAT"
        }
        translateSeg = "\n@SP" +
          "\nA=M-1" +
          "\nD=M" + //D=RAM[RAM[SP]-1]
          pointerNum + //@THAT OR @THIS
          "\nM=D" + //RAM[POINTER] = RAM[RAM[SP]-1]
          "\n@SP" +
          "\nM=M-1\n"
      case default => translateSeg = ""
    }
    return translateSeg
  }
}