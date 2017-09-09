import org.scalatest.{Matchers, WordSpec}

class LcsSpec extends WordSpec with Matchers {
  import lcs._

  "LCS" must {

    "find only longest sequence available" in {
      lcs("ABABABABDAAAAAAC", "ABCDAAAAAC") should be ("ABDAAAAAC")
    }

    "find sequence that is common for two given strings" in {
      lcs("AAAAACCA", "ABC") should be ("AC")
    }

    "return empty string if there is no available solution" in {
      lcs("B", "A") should be ("")
    }
  }

}
