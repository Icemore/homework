import scala.util.parsing.combinator.JavaTokenParsers

object json extends App {

  class JsonParser extends JavaTokenParsers {
    def jsonObject : Parser[Map[String, Any]] =
      "{" ~> repsep(jsonString ~ ":" ~ jsonValue, ",") <~ "}" ^^ {li => Map() ++ li.map{case str ~ ":" ~ value => (str, value)}}

    def jsonArray : Parser[List[Any]] =
      "[" ~> repsep(jsonValue, ",") <~ "]"

    def jsonValue : Parser[Any] =
      jsonObject | jsonArray | jsonString | jsonNumber | "true" ^^ {_ => true} | "false" ^^ {_ => false} | "null" ^^ {_ => null}

    def jsonString : Parser[String] = stringLiteral
    def jsonNumber : Parser[Float] = floatingPointNumber ^^ {f => f.toFloat}

    def json : Parser[Any] = jsonValue
  }

  val test =
    """
      |{
      |  "Image": {
      |      "Width":  800,
      |      "Height": 600,
      |      "Title":  "View from 15th Floor",
      |      "Thumbnail": {
      |          "Url":    "http://www.example.com/image/481989943",
      |          "Height": 125,
      |          "Width":  100
      |      },
      |      "Animated" : false,
      |      "IDs": [116, 943, 234, 38793]
      |    }
      |}    """.stripMargin


  val parser = new JsonParser
  val json = parser.parseAll(parser.json, test)

  json match {
    case parser.Success(res, _) => println(res)
    case parser.NoSuccess(msg, _) => println("failed: " + msg)
  }
}

