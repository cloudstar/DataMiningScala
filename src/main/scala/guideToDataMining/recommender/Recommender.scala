package guideToDataMining.recommender

import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode


case class User(name: String, map: Map[String, Double])

object Recommender {

  def
  main(args: Array[String]): Unit = {
    println(computeNNs("Hailey", users))
    println(recommend("Hailey", users))
    println(recommendWeighted("Hailey", users))
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

  /** Computes the manhattenDistance between two ratings
    *
    * @param rating1
    * @param rating2
    * @return None if one rating is note defined, else the distance
    */
  def manhattan(rating1: Option[Map[String, Double]], rating2: Option[Map[String, Double]]): Option[Double] = {

    //distance  or if not rated 0
    def calcDistToRat2(rat: (String, Double)) =
      math.abs(rat._2 - rating2.get.getOrElse(rat._1, rat._2))

    if (rating1.isEmpty
      || rating2.isEmpty
      || rating1.get.map(e => rating2.get.contains(e._1)).reduce(_ & _))
      None //  None if no rating or no rating in common
    else
      Option((rating1.get map (calcDistToRat2(_))).sum)


  }

  /** Computes the minkowski distance between two ratings
    *
    * @param ratings1
    * @param ratings2
    * @return None if one rating is not defined, else the distance
    */
  def minkowski(ratings1: Option[Map[String, Double]],
                ratings2: Option[Map[String, Double]],
                r: Double): Option[Double] = {

    def distanceOrNone(rat1: (String, Double)): Double =
      ratings2.get.get(rat1._1) match {
        case Some(rating) => math.pow(math.abs(rat1._2 - rating), r)
        case None => 0
      }

    if (ratings1.isEmpty || ratings2.isEmpty) None //no rating
    else Option(math.pow((ratings1.get map (e => distanceOrNone(e))).sum, 1 / r))

  }

  /** Computes the Pearson correlation coefficient with a one-pass approximation
    * https://en.wikipedia.org/wiki/Pearson_correlation_coefficient#For_a_sample
    *
    * @param rating1
    * @param rating2
    * @return None if rating not defined, correlation with [-1;1] otherwise
    */
  def pearsonCorrCoeff(rating1: Option[Map[String, Double]], rating2: Option[Map[String, Double]]): Option[Double] = {
    var n = 0D
    var sum_rat1Xrat2 = 0D
    var sum_rat1 = 0D
    var sum_rat2 = 0D
    var sum_sq_rat1 = 0D
    var sum_sq_rat2 = 0D

    def passSums(r1: Double, r2: Double): Unit = {
      n += 1
      sum_rat1Xrat2 += r1 * r2
      sum_rat1 += r1
      sum_rat2 += r2
      sum_sq_rat1 += (r1 * r1)
      sum_sq_rat2 += (r2 * r2)
    }

    def calc: Double = (n * sum_rat1Xrat2 - sum_rat1 * sum_rat2) /
      (math.sqrt(n * sum_sq_rat1 - sum_rat1 * sum_rat1) * math.sqrt(n * sum_sq_rat2 - sum_rat2 * sum_rat2))

    if (rating1.isEmpty || rating2.isEmpty) None //no rating
    else rating1.get foreach (
      rat1 => rating2.get.get(rat1._1)
      match {
        case Some(rat2) => passSums(rat1._2, rat2)
        case None => //skip
      })
    Option(calc)
  }

  /** Finds the nearest neighbor by distance.
    *
    * Computes all Manhatten-Distances and return them in an ordered list.
    * If the user is in List, his distance will be Zero.
    *
    * @param fromUser
    * @param users all users, with
    * @return list of all users ordered by distance, 0 if s
    */
  def computeNNs(fromUser: String, users: Map[String, Map[String, Double]]): List[(String, Double)] = {
    val userRatings = users get fromUser
    users.map(user => {
      manhattan(Some(user._2), userRatings) match {
        case Some(rating) => Some(user._1, rating)
        case None => None
      }
    }).flatten.toList.sortBy(t => t._2)
  }

  /** Recommends artists by not rated nearest neighbor ratings
    *
    * @param user
    * @param users
    * @return
    */
  def recommend(user: String, users: Map[String, Map[String, Double]]): List[(String, Double)] = {
    val nearestUsers = computeNNs(user, users)
    if (nearestUsers.size < 2) List[(String, Double)]()
    else {
      val ratings = users.getOrElse(user, Map[String, Double]())

      users.get(nearestUsers(1)._1) match {
        case Some(neighbor) => neighbor.toList collect { case (id, rating) if ratings.get(id).isEmpty => (id, rating) }
        case None => List[(String, Double)]()
      }
    }
  }

  def recommendWeighted(user: String, data: Map[String, Map[String, Double]]): List[(String, Double)] = {

    case class RatingAcc(sum_rat: Double = 0, norm_rat: Double = 0)

    val accRatings = mutable.Map[String, RatingAcc]()
    val userRatings = data(user)
    val allNeighbors = computeNNs(user, data)


    def updateAccRating(item: String, rat: Double, dist: Double) = {
      accRatings.get(item) match {
        case Some(acc) => accRatings.put(item, RatingAcc((acc.sum_rat + rat * dist), acc.norm_rat + dist))
        case None => accRatings.put(item, RatingAcc(rat, dist))
      }
    }

    def normalize(sum: Double, norm: Double) =
      BigDecimal.apply(sum / norm).setScale(1, RoundingMode.HALF_UP).doubleValue()

    allNeighbors foreach (neighbor => {
      data(neighbor._1) foreach (rating => {
        userRatings.get(rating._1) match {
          case Some(_) => //allready has a rating for the item
          case None => updateAccRating(rating._1, rating._2, neighbor._2)
        }
      })
    })
    accRatings.map(e => (e._1, normalize(e._2.sum_rat, e._2.norm_rat))) //normalize rating
      .toList
  }

  def recommendWeightedPear(user: String, data: Map[String, Map[String, Double]]): List[(String, Double)] = {

    case class RatingAcc(sum_rat: Double = 0, norm_rat: Double = 0)

    val accRatings = mutable.Map[String, RatingAcc]()
    val userRatings = data(user)
    val allNeighbors = computeNNs(user, data)


    def updateAccRating(item: String, rat: Double, dist: Double) = {
      accRatings.get(item) match {
        case Some(acc) => accRatings.put(item, RatingAcc((acc.sum_rat + rat * dist), acc.norm_rat + dist))
        case None => accRatings.put(item, RatingAcc(rat, dist))
      }
    }

    def normalize(sum: Double, norm: Double) =
      BigDecimal.apply(sum / norm).setScale(1, RoundingMode.HALF_UP).doubleValue()

    allNeighbors foreach (neighbor => {
      val p = pearsonCorrCoeff(Option(userRatings), Option(data(neighbor._1))) match {
        case Some(p) => p
        case None => 0
      }
      data(neighbor._1) foreach (otherRating => {
        userRatings.get(otherRating._1) match {
          case Some(_) => //allready has a rating for the item
          case None => updateAccRating(otherRating._1, otherRating._2, p)
        }
      })
    })
    accRatings.map(e => (e._1, normalize(e._2.sum_rat, e._2.norm_rat))) //normalize rating
      .toList
  }
}