/*
  Exercise_2 By Tahel El Al (324012129) and Adva Cohen (323840561)
  From VM to ASM functions commends and program flow.
 */

package compilers
import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source
import compilers.VM2ASM

object Main {

  var curFile : String = ""

  def main(args: Array[String]): Unit = {
    val file = new File("C:\\Users\\tahel\\Documents\\AAA_Study\\Ecronot\\Levels\\Exersice_2")
    val files = file.listFiles()
    var translationLine : String = " "

    var asmfname : String = "asmFile.asm"
    val writer = new PrintWriter(new FileOutputStream(asmfname,true))

    for (f <- files) {
      if (f.getName().endsWith(".vm")) {

        curFile = f.getName() //className

        for (line <- Source.fromFile(f).getLines())
        {
          var curWordList = line.split(" ")
          var cmd = curWordList(0)

          cmd match {

            //Memory access cases
            case "push" => translationLine = VM2ASM.push( curWordList(1), curWordList(2),curFile)
            case "pop" => translationLine = VM2ASM.pop( curWordList(1), curWordList(2),curFile)

            //arithmetics cases
            case "add" => translationLine = VM2ASM.add()
            case "sub" => translationLine = VM2ASM.sub()

            //boolean cases
            case "eq" => translationLine = VM2ASM.eq(curFile)
            case "neg" => translationLine = VM2ASM.neg()
            case "gt" => translationLine = VM2ASM.gt(curFile)
            case "lt" => translationLine = VM2ASM.lt(curFile)
            case "and" => translationLine = VM2ASM.and()
            case "or" => translationLine = VM2ASM.or()
            case "not" => translationLine = VM2ASM.not()

            //program flow
            case "label" => translationLine = VM2ASM.label(curWordList(1))
            case "goto" => translationLine = VM2ASM.goto(curWordList(1), curFile)
            case "if-goto" => translationLine = VM2ASM.ifGoto(curWordList(1), curFile)

            //functions commends
            case "function" => translationLine = VM2ASM.function(curWordList(1), curWordList(2))
            case "call" => translationLine = VM2ASM.call(curWordList(1), curWordList(2), curFile)
            case "return" => translationLine = VM2ASM.returnFunc(curWordList(1),curFile)
            case default => translationLine = ""
          }
          writer.write(translationLine)
        }
        writer.close()
      }
    }
  }

}