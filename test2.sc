val arr:Seq[Map[String,Int]] = Seq(Map("a_0"->10),Map("a_0"->15),Map("a_1"->20),Map("a_0"->25),Map("a_1"->30))
arr.map(_.keys.head)

val mySeq1 = List(0,1,0,0,0,1,1,0,0,1,0)
val mySeq2 = List(1,1,0,0,0,1,1,0,0,1,0)
val mySeq3 = List(1,1,1,1)
val mySeq4 = List(0,0,0,0,0)
val mySeq5 = List(1,1,0,0,0,1,1,0,0,1,1)

val res = mySeq5.foldLeft(List.empty[List[Int]]) {
//  case (acc, i) if acc.isEmpty => List(List(i))
  case (acc,1) if acc.isEmpty  => List() :+ List(1) :+ Nil
  case (acc,0) if acc.isEmpty  => List() :+ List(0)
  case (acc, 1) => acc.init :+ (acc.last :+ 1) :+ Nil
  case (acc, 0) => acc.init :+ (acc.last :+ 0)
  case _        => Nil
}




arr.foldLeft(List.empty[List[Map[String,Int]]]) {
//  case (acc,i) if acc.isEmpty => List(List(i))
    case(acc,i) if acc.isEmpty & i.keys.head.endsWith("_1") => List() :+ List(i) :+ Nil
    case(acc,i) if acc.isEmpty & i.keys.head.endsWith("_0") => List() :+ List(i)
    case (acc,i) if i.keys.head.endsWith("_1") => acc.init :+ (acc.last :+ i) :+ Nil
    case (acc,i) if i.keys.head.endsWith("_0") => acc.init :+ (acc.last :+ i)
    case _                                     => Nil
}














//val mySeq = List(0, 1, 2, 1, 0, -1, 0, 1, 2, 3, 2)
//mySeq.foldLeft(List.empty[List[Int]]) {
//  case (acc, i) if acc.isEmpty => List(List(i))
//  case (acc, 0) => acc :+ List(0)
//  case (acc, i) => acc.init :+ (acc.last :+ i)
//}

