package zhenghui

import scala.util.Try

/**
 * User: zhenghui
 * Date: 15-11-3
 * Time: 下午2:29
 */
object IPv4Util {

  val HEX_NUM = 16
  val BINARY_NUM= 2

  def checksum(header: String): String = {
    header.split("\\s+") match {
      case Array(Numner10(p1),Numner10(p2),Numner10(p3),Numner10(p4),Numner10(p5),Numner10(p6),Numner10(p7),Numner10(p8),Numner10(p9),Numner10(p10)) =>
        p1+p2+p3+p4+p5+p7+p8+p9+p10 match {
          case BinaryTuple(head,tail) =>
            val checkCode = BigInt(Integer.valueOf(fill0((head + tail).toString(BINARY_NUM),16).map(c => if(c == '1')'0'else '1').toString,BINARY_NUM))
            List(p1,p2,p3,p4,p5,checkCode,p7,p8,p9,p10).map(s=>fill0(s.toString(HEX_NUM),4)).mkString(" ")
          case _ => "" //ignore
        }
      case _ =>
        ""
    }
  }

  def fill0(str:String,num:Int):String = {
    if(str.length >= num) str else {
      var remainders = num - str.length
      var reverseStr = str.reverse
      while(remainders > 0){
        reverseStr += "0"
        remainders -= 1
      }
      reverseStr.reverse
    }
  }

  object Numner10 {
    def unapply(str:String): Option[BigInt] ={
      Try{Integer.parseInt(str,HEX_NUM)}.toOption.flatMap(i => Some(BigInt(i)))
    }
  }

  object BinaryTuple{
    def unapply(bi : BigInt):Option[(BigInt,BigInt)] = {
      Try{
        val biBinary = bi.toString(BINARY_NUM)
        val subNum = if(biBinary.length%4== 0) 4 else biBinary.length%4
        (BigInt(Integer.valueOf(biBinary.substring(0,subNum),BINARY_NUM)),BigInt(Integer.valueOf(biBinary.substring(subNum),BINARY_NUM)))
      }.toOption
    }
  }

  def main(args: Array[String]) {
    println(checksum("4500 0042 3038 0000 4011 0000 c0a8 0afa af90 6ce9"))
    //"4500 0042 3038 0000 4011 6257 c0a8 0afa af90 6ce9"
  }
}
