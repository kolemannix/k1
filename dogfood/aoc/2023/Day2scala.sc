import java.nio.file.Path

case class Supply(red: Int, green: Int, blue: Int)
enum Color { case Red, Green, Blue }
case class Value(amount: Int, color: Color)

val input: String = java.nio.file.Files.readString(Path.of("/Users/knix/dev/k1/dogfood/aoc/2023/2.txt"))

println("Hello, day 2!")
val supply = Supply(
  red= 12,
  green= 13,
  blue= 14
)
var goodGamesSum = 0
input.split("\n").zipWithIndex.foreach { case (lline, index) =>
  val id = index + 1
  val line = lline.split(':')(1)
  var isGoodGame = true
  val rounds = line.split(";").map { round =>
    val valuesRaw = round.split(',')
    val values: Array[Value] = valuesRaw.map { value =>
      val amountColor = value.split(' ').filter(_.nonEmpty)
      println(amountColor.toSeq)
      val amount = amountColor(0).toIntOption.get
      val color: Color = amountColor(1) match {
        case "red" => Color.Red
        case "green" => Color.Green
        case "blue" => Color.Blue
        case c => throw new Exception("Unexpected color: " + c)
      }
      Value(amount, color)
    }
    values.foreach { value => 
      val colorSupply = value.color match {
        case Color.Red => supply.red
        case Color.Green => supply.green
        case Color.Blue => supply.blue
      }
      if (value.amount > colorSupply) {
        isGoodGame = false
        // println(s"Bad ${value.amount}${value.color.toString}")
      }
    }
  }
  if (isGoodGame) { goodGamesSum = goodGamesSum + id }
  // println(line)
}
println(goodGamesSum)

