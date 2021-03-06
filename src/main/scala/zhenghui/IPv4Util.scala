package zhenghui

import scala.util.Try

/**
 * User: zhenghui
 * Date: 15-11-3
 * Time: 下午2:29
 * https://en.wikipedia.org/wiki/IPv4_header_checksum 
 */
object IPv4Util {

  val HEX_NUM = 16
  val BINARY_NUM= 2

  def checksum(header: String): String = {
    header.split("\\s+") match {
      case Array(Numner10(p1),Numner10(p2),Numner10(p3),Numner10(p4),Numner10(p5),Numner10(p6),Numner10(p7),Numner10(p8),Numner10(p9),Numner10(p10)) =>
        p1+p2+p3+p4+p5+p7+p8+p9+p10 match {
          case BinaryTuple(head,tail) =>
            val checkCode = BigInt(Integer.valueOf((head + tail).toString(BINARY_NUM).reverse.padTo(16,'0').reverse.map(c => if(c == '1')'0'else '1').toString,BINARY_NUM))
            List(p1,p2,p3,p4,p5,checkCode,p7,p8,p9,p10).map(_.toString(HEX_NUM).reverse.padTo(4,'0').reverse).mkString(" ")
          case _ => "" //ignore
        }
      case _ =>
        ""
    }
  }

  object Numner10 {
    def unapply(str:String): Option[BigInt] = Try{Integer.parseInt(str,HEX_NUM)}.toOption.flatMap(i => Some(BigInt(i)))
  }

  object BinaryTuple{
    def unapply(bi : BigInt):Option[(BigInt,BigInt)] = {
      Try{
        val biBinary = bi.toString(BINARY_NUM).reverse.padTo(20,'0').reverse
        (BigInt(Integer.valueOf(biBinary.take(4),BINARY_NUM)),BigInt(Integer.valueOf(biBinary.drop(4),BINARY_NUM)))
      }.toOption
    }
  }

  def main(args: Array[String]) {
//    assert(checksum2("4500 0073 0000 4000 4011 0000 c0a8 0001 c0a8 00c7") == "4500 0073 0000 4000 4011 b861 c0a8 0001 c0a8 00c7")
    val i = 111
    println(i & 0xffff)

  }

  def checksum2(header: String): String = {
    val bytes = header.split(" ").toArray
    val sum = bytes.map(byte => Integer.parseInt(byte, 16)).sum
    val checksum = Iterator.iterate(sum) { s =>
      (s & 0xffff) + ((s >> 16) & 0xffff)
    } find (_ <= 0xffff)
    bytes(5) = ((~checksum.get) & 0x0ffff).toHexString

    bytes.mkString(" ")
  }

}
