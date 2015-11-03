package zhenghui

import org.scalatest.{Matchers, FlatSpec}

/**
 * User: zhenghui
 * Date: 15-11-3
 * Time: 下午6:54
 */
class IPv4UtilTest extends FlatSpec with Matchers{

  it should "equals " in {
    IPv4Util.checksum("4500 0073 0000 4000 4011 0000 c0a8 0001 c0a8 00c7") should be("4500 0073 0000 4000 4011 b861 c0a8 0001 c0a8 00c7")

    IPv4Util.checksum("4500 0042 3038 0000 4011 0000 c0a8 0afa af90 6ce9") should be("4500 0042 3038 0000 4011 6257 c0a8 0afa af90 6ce9")

  }

}
