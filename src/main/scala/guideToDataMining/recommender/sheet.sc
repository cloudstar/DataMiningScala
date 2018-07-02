
//val dist = new Distance

val a = List(1,2,3,5)
val b = List(Some(1),Some(2),None)

val users: Map[String, Map[String, Double]] = Map(
  ("Angelica", Map("BT" -> 3.5, "BB" -> 2.0, "NJ" -> 4.5, "P" -> 5.0, "SS" -> 1.5, "TS" -> 2.5, "VW" -> 2.0)),
  ("Bill", Map("BT" -> 2.0, "BB" -> 3.5, "D" -> 4.0, "P" -> 2.0, "SS" -> 3.5, "VW" -> 3.0)),
  ("Chan", Map("BT" -> 5, "BB" -> 1, "D" -> 1, "NJ" -> 3, "P" -> 5, "SS" -> 1)),
  ("Dan", Map("BT" -> 3, "BB" -> 4, "D" -> 4.5, "P" -> 3, "SS" -> 3.5, "TS" -> 4, "VW" -> 2)),
  ("Hailey", Map("BB" -> 4D, "D" -> 1D, "NJ" -> 4D, "TS" -> 4D, "VW" -> 1D)),
  ("Jordyn", Map("BB" -> 4.5, "D" -> 4, "NJ" -> 5, "P" -> 5, "SS" -> 4.5, "TS" -> 4, "VW" -> 4)),
  ("Sam", Map("BT" -> 5, "BB" -> 2, "NJ" -> 3, "P" -> 5, "SS" -> 4, "TS" -> 5)),
  ("Veronica", Map("BT" -> 3D, "NJ" -> 5D, "P" -> 4D, "SS" -> 2.5D, "TS" -> 3D)))


def manhattan(rating1: Option[Map[String, Double]], rating2: Option[Map[String, Double]]): Option[Double] =
  if (!rating1.isDefined || !rating2.isDefined)
    None
  else
    Option(rating1.get map (e => math.abs(e._2 - rating2.get.getOrElse(e._1, e._2))) reduce (_ + _))


manhattan(users.get("Bill"),users.get("Sam"))

println("dd")
users.map(user => {
  (manhattan(Some(user._2), users get "Bill"))  match {
    case Some(rating) => Some(user._1, rating)
    case None => None
  }}).toList.flatten.sortBy(t => t._2)

Option(None)