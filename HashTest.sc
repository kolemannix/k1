import scala.collection.mutable.HashMap

val table = HashMap.empty[String, Int]
(0 until 100000).foreach { x =>
  table.put(x.toString, x)
}

assert(table.size == 100000)

(0 until 100000).foreach { x =>
  val elem = table(x.toString)
  assert(elem == x)
}
println(table.size)
