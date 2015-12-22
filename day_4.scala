import java.security.MessageDigest

object Hex {
  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
}

object Brutforcer {

  val digester = MessageDigest.getInstance("md5")

  def md5Hex(input: String) = {
    Hex.valueOf(digester.digest(input.getBytes))
  }

  def brutforceMD5(input: String, nullCount: Int) = {

    def loop(appendix: Int): String = {
      val key = input + appendix.toString
      if (md5Hex(key).startsWith("0" * nullCount)) key
      else loop(appendix + 1)
    }

    loop(0)
  }

  def main(args: Array[String]) {
    println(brutforceMD5("ckczppom", 5))
    println(brutforceMD5("ckczppom", 6))
  }

}



