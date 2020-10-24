import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.math.abs
object WeekDaysFunctions {

  def forWeekDays(days: List[String]): String = {
    var str = "";

    for (day <- days) {
      str += day + ",";
    }
    return str;
  }

  def forWeekDaysWithP(days: List[String]): String = {
    var str = "";

    for (day <- days if day.startsWith("p")) {
      str += day + ",";
    }
    return str;
  }

  def whileWeekDays(days: List[String]): String = {
    var str = "";
    var it = days.iterator;
    while (it.hasNext) {
      str += it.next() + ",";
    }
    return str;
  }
  def recursiveWeekDays(days: List[String], string: String = ""): String = {
    var it = days.iterator;
    var str: String = string;
    if (it.hasNext) {
      str += it.next() + ",";
    }
    if (it.hasNext) {
      return this.recursiveWeekDays(it.toList, str);
    } else
      return str;
  }
  def reverseRecursiveWeekDays(
      days: List[String],
      string: String = ""
  ): String = {
    var list = days.reverse
    var it = list.iterator;
    var str: String = string;
    if (it.hasNext) {
      str += it.next() + ",";
    }
    if (it.hasNext) {
      return this.recursiveWeekDays(it.toList, str);
    } else
      return str;
  }

  @tailrec
  def tailRecursiveWeekDays(days: List[String], string: String = ""): String = {
    val nextElement = days.head
    val nextResult = string.concat(nextElement + ",")
    if (nextElement == days.last) return nextResult;
    else return tailRecursiveWeekDays(days.tail, nextResult);
  }
  def foldlWeekDays(days: List[String]): String = {
    return days.foldLeft("")((m, n) => m + n + ",");
  }

  def foldrWeekDays(days: List[String]): String = {
    return days.foldRight("")((m, n) => m + "," + n);
  }

  def foldlWeekDaysWithP(days: List[String]): String = {
    return days.foldLeft("")((m, n) =>
      m + (if (n.startsWith("p")) n + "," else "")
    );
  }
}

object CollectionFunctions {

  def discount(
      map: Map[String, Double],
      percentage: Double
  ): Map[String, Double] =
    map.view.mapValues(value => value - value * percentage).toMap

  def tupleToString(tuple: (String, Int, Boolean)): String =
    "String: " + tuple._1 + ", Int: " + tuple._2 + ", Boolean: " + tuple._3

  def getPriceOfProduct(
      productMap: Map[String, Double],
      productName: String
  ): Option[Double] = productMap.get(productName)

  def filterZeros(list: List[Int], retList: List[Int] = List()): List[Int] = {
    var it = list.reverse.iterator;
    var retl = retList;
    if (it.hasNext) {
      var next = it.next()
      if (next != 0){
        retl = next :: retl
      }
    }
    if (it.hasNext) {
      return this.filterZeros(it.toList.reverse, retl);
    } else
      return retList;
  }
  def increment(list: List[Int]): List[Int] = {
    list.map(element => element + 1)
  }
  def absolute(list: List[Double]): List[Double] = {
    list.filter(element => element < 12 && element > -5 )
      .map(element => abs(element))
  }
}

object Main extends App {
  var test = "";
  val days: List[String] = List(
    "poniedziałek",
    "wtorek",
    "środa",
    "czwartek",
    "piątek",
    "sobota",
    "niedziela"
  )

  var map = Map(
    "wine" -> 10.99,
    "beer" -> 2.99,
    "vodka" -> 25.99
  )
  var assignments = SortedMap(
    1 -> SortedMap(
      "a" -> WeekDaysFunctions.forWeekDays(days),
      "b" -> WeekDaysFunctions.forWeekDaysWithP(days),
      "c" -> WeekDaysFunctions.whileWeekDays(days)
    ),
    2 -> SortedMap(
      "a" -> WeekDaysFunctions.recursiveWeekDays(days),
      "b" -> WeekDaysFunctions.reverseRecursiveWeekDays(days)
    ),
    3 -> WeekDaysFunctions.tailRecursiveWeekDays(days),
    4 -> SortedMap(
      "a" -> WeekDaysFunctions.foldlWeekDays(days),
      "b" -> WeekDaysFunctions.foldrWeekDays(days),
      "c" -> WeekDaysFunctions.foldlWeekDaysWithP(days)
    ),
    5 -> CollectionFunctions.discount(map, 0.10),
    6 -> CollectionFunctions.tupleToString("str", 1, true),
    7 -> CollectionFunctions.getPriceOfProduct(map, "wine").toString,
    8 -> CollectionFunctions
      .filterZeros(
        List(0, 1, 2, 3, 4, 0, 0, 1, 2, 3, 4, 0)
      )
      .mkString(" "),
    9 -> CollectionFunctions
      .increment(
        List(0, 1, 2, 3, 4, 0, 0, 1, 2, 3, 4, 0)
      )
      .mkString(" "),
    10 -> CollectionFunctions
      .absolute(
        List(0.5, 0.1, 92.3, 2.3, 4.5, 0.1, 0.3, -27.5, -30, 20.3, 4, 0)
      ).mkString(" ")
  )

  for (assignment <- assignments) {
    if (assignment._2.getClass() != test.getClass())
      for (sub <- assignment._2.asInstanceOf[Map[String, Map[String, String]]])
        println(assignment._1 + ". " + sub._1 + ": " + sub._2)
    else
      println(assignment._1 + ": " + assignment._2)
  }
}
