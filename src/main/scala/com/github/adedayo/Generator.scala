package com.github.adedayo

import java.util.regex.Pattern

import scopt.OptionParser

/**
 * @author Adedayo Adetoye
 *
 */

object Generator {
  val NAME = "passgen"
  val VERSION = "1.0.0"
  val transforms = List("leet", "leetAdvanced", "leetUltimate", "case", "expand", "pad", "padLeft", "padRight")

  /**
   * Generates passwords from a base set of passwords by applying various transforms on the base set and/or by expanding
   * the base set with supplied "expander" characters
   * @param transforms the transforms to apply to the base password set
   * @param word a single base word used to derive passwords
   * @param expander a list of strings that can be used to expand generated passwords such as prefixes, suffixes, pads etc
   * @return list of generated passwords
   */
  def generatePasswords(transforms: List[String], word: String, expander: List[String] = List[String](), regexSafe: Boolean = false): List[String] = {
    generate(transforms, List(word), expander, regexSafe)
  }

  /**
   * Generates passwords from a base set of passwords by applying various transforms on the base set and/or by expanding
   * the base set with supplied "expander" characters
   * @param transforms the transforms to apply to the base password set
   * @param words the base list of words that will be used to derive candidate passwords - use common words:
   *              company names, product names etc to generate good password candidates during a penetration assessment
   * @param expander a list of strings that can be used to expand generated passwords such as prefixes, suffixes, pads etc
   * @return
   */
  def generate(transforms: List[String], words: List[String], expander: List[String] = List[String](), regexSafe: Boolean = false): List[String] = {
    if (transforms.isEmpty) {
      if (regexSafe)
      //        words.map(_.replaceAll( """\\""", """\\\\""").
      //          replaceAll( """\(""", """\\(""")
      //          .replaceAll( """\)""", """\\)""")
      //          .replaceAll( """\|""", """\\|""")
      //        )
        words.map(Pattern.quote(_))
      else
        words
    }
    else {
      val out = transforms.head match {
        case "leet" => genLeet(words, Leet.leet)
        case "leetAdvanced" => genLeet(words, LeetAdvanced.leet)
        case "leetUltimate" => genLeet(words, LeetUltimate.leet)
        case "case" => genCase(words)
        case "expand" => genExpand(words, expander)
        case "pad" => genPadding(words, expander)
        case "padLeft" => genLeftPadding(words, expander)
        case "padRight" => genRightPadding(words, expander)
        case _ => words
      }
      generate(transforms.tail, out, expander, regexSafe)
    }
  }

  private def genLeet(basePasswords: List[String], func: Char => List[String]): List[String] = {
    val result = basePasswords.map(pwd => {
      val z = pwd.map(char => {
        (char.toString :: func(char)).distinct
      })
      z
    })
    product(result.flatten).map(_.mkString(""))
  }

  private def genExpand(basePasswords: List[String], expander: List[String]): List[String] = {
    val result = basePasswords.map(pwd => {
      val z = pwd.map(char => {
        expander.combinations(1).map(x => List(char.toString + x.mkString(""), x.mkString("") + char.toString)).toList.flatten
      })
      z

    })
    product(result.flatten).map(_.mkString("")).distinct
  }

  private def genPadding(basePasswords: List[String], expander: List[String]): List[String] = {
    val perm = (expander.permutations.toList ::: (for (i <- 1 until expander.length) yield expander.combinations(i)).flatten.toList).map(_.mkString(""))
    product(List(perm) ::: List(basePasswords) ::: List(perm)).map(_.mkString(""))
  }

  private def genLeftPadding(basePasswords: List[String], expander: List[String]): List[String] = {
    val perm = (expander.permutations.toList ::: (for (i <- 1 until expander.length) yield expander.combinations(i)).flatten.toList).map(_.mkString(""))
    product(List(perm) ::: List(basePasswords)).map(_.mkString(""))
  }

  private def genRightPadding(basePasswords: List[String], expander: List[String]): List[String] = {
    val perm = (expander.permutations.toList ::: (for (i <- 1 until expander.length) yield expander.combinations(i)).flatten.toList).map(_.mkString(""))
    product(List(basePasswords) ::: List(perm)).map(_.mkString(""))
  }

  private def genCase(basePasswords: List[String]): List[String] = {
    val result = basePasswords.map(pwd => {
      val z = pwd.map(char => {
        val c = char.toString
        List(c.toLowerCase, c.toUpperCase).distinct
      })
      z
    })
    product(result.flatten).map(_.mkString(""))
  }

  import scalaz.Scalaz._

  def product(list: List[List[String]]) = list.sequence[List, String]


  case class Config(transforms: Seq[String] = Seq(), pads: Seq[String] = Seq(), words: Seq[String] = Seq(), regexsafe: Boolean = false)

  def main(args: Array[String]) {

    val parser = new OptionParser[Config](NAME) {

      head(NAME, VERSION)

      note(
        s"""DESCRIPTION
           |
           |$NAME provides a simple command line utility and API for generating possible passwords based on various
                   |transforms of a supplied base set of root words. It is based on the premise that people are bad at remembering
                   |good passwords, and therefore resort to rules as a mental shortcut to generating and remembering passwords.
                   |This tool attempts to exploit that weakness through interfaces that help you quickly generate candidate passwords.
                   |For example, names of companies, people, places, sports clubs etc. can be used to generate likely passwords
                   |for your next penetration assessment to strengthen your organisation's security posture: consider any password
                   |generated by this tool as weak.
                   |
                   |The following options are available:
        """.stripMargin)



      opt[Seq[String]]('t', "transforms") valueName ("<transform1>,<transform2>...") action { (t, c) =>
        c.copy(transforms = t)
      } text s"""transformation(s) to perform on the base word(s). Available transforms include: ${transforms.mkString(", ")}"""


      opt[Seq[String]]('p', "pads") valueName ("<pad1>,<pad2>...") action { (p, c) =>
        c.copy(pads = p)
      } text ("characters used to pad the password. Use with one of the padding transforms")


      opt[Seq[String]]('w', "words") valueName ("<word1>,<word2>...") action { (w, c) =>
        c.copy(words = w)
      } text ("word(s) to be used to derive the passwords - use common names and terms to generate good password guesses")

      opt[Unit]("regex-safe") abbr ("rs") action { (_, c) =>
        c.copy(regexsafe = true)
      } text ("make the output safe for use in regular expressions")

      help("help") text ("prints this usage text")
      version("version") text ("prints version number")
      note(
        s"""
           |EXAMPLES
           |
           |$NAME -t leetUltimate -w Password
                   |generates the "leetUltimate" transform of the word "Password"
                   |
                   |$NAME -t leetUltimate,pad -w Password -p 1,2
                           |generates the "leetUltimate" transform of "Password" and then applies the "pad" transform,
                           |which pads the result with various combinations of "1" and "2" on both side
        """.stripMargin)

      override def showUsageOnError = true
    }

    val result = parser.parse(args, Config()) match {
      case Some(config) => {
        generate(config.transforms.toList, config.words.toList, config.pads.toList.map(_.toString), config.regexsafe)
      }
      case None =>
        List()
    }

    println(result.mkString("\n"))
  }

}
