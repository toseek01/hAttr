val depaturePoint = (arr:Seq[Map[String,Long]],date_start:Long,no_conversion_symbol:String) => {
  val (before,after) = arr.partition(_.values.head < date_start)
  val before_valid = before.reverse.takeWhile(_.keySet.head.endsWith(no_conversion_symbol)).reverse
  val path = before_valid ++ after
  path
}


val test = List("questions", "tags", "users", "badges", "unanswered")

test.foldLeft(List.empty[List[String]]) {
  case ((head :: tail), word) if head.map(_.length).sum + word.length < 12 =>
    (word :: head) :: tail
  case (result, word) =>
    List(word) :: result
}

val test1:List[Map[String,Int]] = List(Map("a_0"->10),Map("b_0"->15),Map("a_0"->20),Map("b_1"->30))

