package guideToDataMining

case class User(name: String, map: Map[String, Double])

class Recommender {


}

object Recommender {

  def main(args: Array[String]): Unit = {
    val d = new Recommender
    println(computeNearestNeighbor("Hailejy", users))
    println(recommend("Hailejy", users))
  }

  val users: Map[String, Map[String, Double]] = Map(
    ("Angelica", Map("BT" -> 3.5, "BB" -> 2.0, "NJ" -> 4.5, "P" -> 5.0, "SS" -> 1.5, "TS" -> 2.5, "VW" -> 2.0)),
    ("Bill", Map("BT" -> 2.0, "BB" -> 3.5, "D" -> 4.0, "P" -> 2.0, "SS" -> 3.5, "VW" -> 3.0)),
    ("Chan", Map("BT" -> 5, "BB" -> 1, "D" -> 1, "NJ" -> 3, "P" -> 5, "SS" -> 1)),
    ("Dan", Map("BT" -> 3, "BB" -> 4, "D" -> 4.5, "P" -> 3, "SS" -> 3.5, "TS" -> 4, "VW" -> 2)),
    ("Hailey", Map("BB" -> 4D, "D" -> 1D, "NJ" -> 4D, "TS" -> 4D, "VW" -> 1D)),
    ("Jordyn", Map("BB" -> 4.5, "D" -> 4, "NJ" -> 5, "P" -> 5, "SS" -> 4.5, "TS" -> 4, "VW" -> 4)),
    ("Sam", Map("BT" -> 5, "BB" -> 2, "NJ" -> 3, "P" -> 5, "SS" -> 4, "TS" -> 5)),
    ("Veronica", Map("BT" -> 3D, "NJ" -> 5D, "P" -> 4D, "SS" -> 2.5D, "TS" -> 3D)))

  /**Computes the manhattenDistance between two ratings
    *
    * @param rating1
    * @param rating2
    * @return None if one rating is note defined, else the distance
    */
  def manhattan(rating1: Option[Map[String, Double]], rating2: Option[Map[String, Double]]): Option[Double] = {
    //  None if no rating or no rating in common
    if (!rating1.isDefined || !rating2.isDefined || rating1.get.map(e=> rating2.get.contains(e._1)).reduce(_&_))
      None
    else
      Option(rating1.get map (e => math.abs(e._2 - rating2.get.getOrElse(e._1, e._2))) reduce (_ + _))

  }
  /**Findes the nearest neighbor by distance
    *
    * Computes all manhattenDistances and retunrs them in a ordered list.
    * If the fromUser is in List, his distance will be Zero. If
    * @param fromUser
    * @param users all users, with
    * @return list of all users ordered by distance, 0 if s
    */
  def computeNearestNeighbor(fromUser: String, users: Map[String, Map[String, Double]]): List[(String, Double)] = {
    val userRatings = users get fromUser
    users.map(user => {
      manhattan(Some(user._2), userRatings) match {
        case Some(rating) => Some(user._1, rating)
        case None => None
      }
    }).flatten.toList.sortBy(t => t._2)
  }

  /**Recommends artists by not rated nearest neighbor ratings
    *
    * @param user
    * @param users
    * @return
    */
  def recommend(user: String, users: Map[String, Map[String, Double]]): List[(String, Double)] = {
    val nearestUsers = computeNearestNeighbor(user, users)
    if (nearestUsers.size < 2) List[(String, Double)]()
    else {
      val ratings = (users.get(user)).getOrElse(Map[String, Double]())

      users.get(nearestUsers(1)._1) match {
        case Some(user) => user.toList collect { case (id, rating) if (!ratings.get(id).isDefined) => (id, rating) }
        case None => List[(String, Double)]()
      }
    }

  }
}