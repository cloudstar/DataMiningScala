package guideToDataMining.recommender

import guideToDataMining.Recommender

class RecommenderTest extends org.scalatest.FunSuite {

  val users: Map[String, Map[String, Double]] = Map(
    ("Angelica", Map("BT" -> 3.5, "BB" -> 2.0, "NJ" -> 4.5, "P" -> 5.0, "SS" -> 1.5, "TS" -> 2.5, "VW" -> 2.0)),
    ("Bill", Map("BT" -> 2.0, "BB" -> 3.5, "D" -> 4.0, "P" -> 2.0, "SS" -> 3.5, "VW" -> 3.0)),
    ("Chan", Map("BT" -> 5, "BB" -> 1, "D" -> 1, "NJ" -> 3, "P" -> 5, "SS" -> 1)),
    ("Dan", Map("BT" -> 3, "BB" -> 4, "D" -> 4.5, "P" -> 3, "SS" -> 3.5, "TS" -> 4, "VW" -> 2)),
    ("Hailey", Map("BB" -> 4D, "D" -> 1D, "NJ" -> 4D, "TS" -> 4D, "VW" -> 1D)),
    ("Jordyn", Map("BB" -> 4.5, "D" -> 4, "NJ" -> 5, "P" -> 5, "SS" -> 4.5, "TS" -> 4, "VW" -> 4)),
    ("Sam", Map("BT" -> 5, "BB" -> 2, "NJ" -> 3, "P" -> 5, "SS" -> 4, "TS" -> 5)),
    ("Veronica", Map("BT" -> 3D, "NJ" -> 5D, "P" -> 4D, "SS" -> 2.5D, "TS" -> 3D)))

  test("Distance should be 8") {
    val dist = Recommender.manhattan(users get("Bill"), users get("Sam"))
    assert(dist.get == 8)
  }
  test("Option should be empty") {
    assert(!Recommender.manhattan(users get ("Bill"), users get ("Ute")).isDefined)
    assert(!Recommender.manhattan(users get ("Hans"), users get ("Ute")).isDefined)
  }
  test("get nearestNeighbor") {
    val nn = Recommender.computeNearestNeighbor("Bill",users)
    assert(nn(0) equals ("Dan",4.0))
    assert(nn(1) equals ("Veronica",4.0))
    assert(nn(6) equals ("Chan",14.0))
  }

  test("user not in data"){
    val nn = Recommender.computeNearestNeighbor("NOONE",users)
    assert(nn isEmpty)
  }
  test("recommend for Bill") {
    val recom = Recommender.recommend("Bill",users)
    assert(recom(0) equals ("NJ",5.0))
    assert(recom(1) equals ("TS",3.0))
  }
}
