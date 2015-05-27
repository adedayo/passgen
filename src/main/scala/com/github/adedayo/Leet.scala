package com.github.adedayo

/**
 * @author Adedayo Adetoye
 *
 */

object Leet {
  implicit val transform = Map[Char, List[String]](
    'a' -> List("4"),
    'b' -> List("b"),
    'c' -> List("c"),
    'd' -> List("d"),
    'e' -> List("3"),
    'f' -> List("f"),
    'g' -> List("6"),
    'h' -> List("h"),
    'i' -> List("1"),
    'j' -> List("j"),
    'k' -> List("k"),
    'l' -> List("l"),
    'm' -> List("m"),
    'n' -> List("n"),
    'o' -> List("0"),
    'p' -> List("p"),
    'q' -> List("q"),
    'r' -> List("r"),
    's' -> List("5"),
    't' -> List("7"),
    'u' -> List("u"),
    'v' -> List("v"),
    'w' -> List("w"),
    'x' -> List("x"),
    'y' -> List("y"),
    'z' -> List("z"))

  def leet(key: Char): List[String] = Leeter.leet(key)
}

object LeetAdvanced {
  implicit val transform = Map[Char, List[String]](
    'a' -> List("4"),
    'b' -> List("|3"),
    'c' -> List("("),
    'd' -> List("|)"),
    'e' -> List("3"),
    'f' -> List("|="),
    'g' -> List("9"),
    'h' -> List("|-|"),
    'i' -> List("!"),
    'j' -> List("_|"),
    'k' -> List("|<"),
    'l' -> List("|_"),
    'm' -> List( """/\/\"""),
    'n' -> List( """|\|"""),
    'o' -> List("0"),
    'p' -> List("|D"),
    'q' -> List("q"),
    'r' -> List("|2"),
    's' -> List("5"),
    't' -> List("7"),
    'u' -> List("(_)"),
    'v' -> List( """\/"""),
    'w' -> List( """\/\/"""),
    'x' -> List("><"),
    'y' -> List("`/"),
    'z' -> List("2"))

  def leet(key: Char): List[String] = Leeter.leet(key)
}

object LeetUltimate {
  implicit val transform = Map[Char, List[String]](
    'a' -> List( """4"""),
    'b' -> List( """8"""),
    'c' -> List( """("""),
    'd' -> List( """|)"""),
    'e' -> List( """3"""),
    'f' -> List( """|#"""),
    'g' -> List( """6"""),
    'h' -> List( """|-|"""),
    'i' -> List( """!"""),
    'j' -> List( """_)"""),
    'k' -> List( """|List("""),
    'l' -> List( """1"""),
    'm' -> List( """|\/|"""),
    'n' -> List( """|\|"""),
    'o' -> List( """0"""),
    'p' -> List( """|>"""),
    'q' -> List( """?"""),
    'r' -> List( """|2"""),
    's' -> List( """5"""),
    't' -> List( """+"""),
    'u' -> List( """|_|"""),
    'v' -> List( """\/"""),
    'w' -> List( """\|/"""),
    'x' -> List( """%"""),
    'y' -> List( """`/"""),
    'z' -> List( """7_"""))

  def leet(key: Char): List[String] = Leeter.leet(key)
}

object Leeter {
  def leet(key: Char)(implicit transform: Map[Char, List[String]]): List[String] = {
    val k = key.toLower
    if (transform.contains(k)) (key.toString :: transform(k).filterNot(_ == k.toString)).sorted
    else List(s"$key")
  }
}