package exercises02
object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  val wordRegex = "[\\s.,!?:\\n\\t\\r()]".r
  val numRegex  = "[\\s!?:\\n\\t\\r()]".r
  val number    = "[0-9]+[0-9.,]*".r
  val engRegex  = "[a-z]".r
  def countWords(text: String): Map[String, Int] = {
    val wordsArray: Array[String] = wordRegex
      .split(text)
      .filter(word => !word.isBlank)
      .map(_.toLowerCase())
    val wordsMap: Map[String, Int] = wordsArray.groupBy(word => word).map(w => (w._1, w._2.length))
    wordsMap
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    val wordsArray: Array[String] = wordRegex
      .split(text)
      .filter(word => !word.isBlank && engRegex.findFirstIn(word).isDefined)
      .map(_.toLowerCase())
    val wordsMap: Map[String, Int] = wordsArray.groupBy(word => word).map(w => (w._1, w._2.length))
    wordsMap
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    val wordsArray: Array[String] = numRegex
      .split(text)
      .filter(word => !word.isBlank && number.matches(word))
      .map(_.toLowerCase())
    val wordsMap: Map[String, Int] = wordsArray.groupBy(word => word).map(w => (w._1, w._2.length))
    wordsMap
  }
}