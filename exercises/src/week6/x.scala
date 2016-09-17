package week6

import scala.io.Source

object x {
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  val mnemonics = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String =
    word.toUpperCase map charCode

  def wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = 
    if (number.isEmpty()) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def translate(number: String): Set[String] = 
    encode(number) map (_ mkString " ")
}

object Main extends App {
  println(x.translate("7225247386"))
  println(x.translate("7168267399"))
  println(x.translate("7168071709"))
}