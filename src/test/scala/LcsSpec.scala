import org.scalatest.{Matchers, WordSpec}

class LcsSpec extends WordSpec with Matchers {
  import lcs.{findCommonSequenceMemo, findCommonSequenceStacksafe}

  "LCS" must {

    "produce correct results for a large strings" in {
      findCommonSequenceStacksafe((0 to 1000).map(_ => "AAAAC").mkString(""), "AC") should be ("AC")
      findCommonSequenceStacksafe("AC", (0 to 1000).map(_ => "AAAAC").mkString) should be ("AC")
    }

    "find only longest sequence available" in {
      findCommonSequenceStacksafe("ABABABABDAAAAAAC", "ABCDAAAAAC") should be ("ABDAAAAAC")
      findCommonSequenceMemo("ABABABABDAAAAAAC", "ABCDAAAAAC") should be ("ABDAAAAAC")
    }

    "find sequence that is common for two given strings" in {
      findCommonSequenceStacksafe("AAAAACCA", "ABC") should be ("AC")
      findCommonSequenceMemo("AAAAACCA", "ABC") should be ("AC")
    }

    "return empty string if there is no available solution" in {
      findCommonSequenceStacksafe("B", "A") should be ("")
      findCommonSequenceMemo("B", "A") should be ("")
    }
  }

}
