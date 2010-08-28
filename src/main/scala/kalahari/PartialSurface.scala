package kalahari

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap


case class Point(x : Double, y : Double)
object Point{
  def apply(pt : (Double, Double)) : Point = Point(pt._1, pt._2)
}

case class Triple(x : Double, y : Double, z : Double)

class Curve(points : SortedMap[Double, Double]){

  def xs : List[Double] = points.keysIterator.toList
  def ys : List[Double] = points.valuesIterator.toList

  lazy val spline = new TSSpline(xs.toArray, ys.toArray)

  override def toString = points.toList.mkString(", ")

  def +(x : Double, y : Double) = 
    points.get(x) match {
      case Some(y2) if y == y2 => this
      case Some(y2) => throw new Exception("Can't add point " + (x, y) + ", curve already has point " + (x, y2))
      case None => new Curve(points + (x -> y))
    }

  def isEmpty = points.isEmpty

  def apply(x : Double) = spline(x)

  def isInternal(x : Double) = 
    if (isEmpty)
      false
    else
      xs.head <= x && x <= xs.last
  def distanceFromEnd(x : Double) : Double = {
    if (x <= xs.head)
      (xs.head - x)
    else if (x >= xs.last)
      (x - xs.last)
    else
      throw new Exception(x + " is an interior point")
  }
}

class Surface(val points : Iterable[(Double, Double, Double)]){
  // xCurves run parallel to the x-axis
  lazy val (xCurves, yCurves) = {
    var xCurves = TreeMap[Double, Curve]()
    var yCurves = TreeMap[Double, Curve]()
    points.foreach{
      case (x, y, z) =>
        xCurves += y -> (xCurves.getOrElse(y, Curve()) + (x, z))
        yCurves += x -> (yCurves.getOrElse(x, Curve()) + (y, z))
    }
    (xCurves, yCurves)
  }

  def fillInGaps : Surface = {
    val allPoints = for ((y, xCurve) <- xCurves;
         (x, yCurve) <- yCurves)
    yield {
      val z0 = xCurve(x)
      val z1 = yCurve(y)
      val z = (xCurve.isInternal(x), yCurve.isInternal(y)) match {
        case (true, true) => (z0 + z1) / 2
        case (true, _) => z0
        case (_, true) => z1
        case _ => Curve.extrapolate(xCurve, yCurve, (x, y))
      }
      (x, y, z)
    }
    new Surface(allPoints)
  }

  def apply(x0 : Double, y0 : Double) = {
    val xCurve = Curve(yCurves.toList.map{
      case (x, yCurve) => {
        (x, yCurve(y0))
      }
    } : _*)
    xCurve(x0)
  }

  override def toString = "x Curves " + xCurves.toList.mkString("; ") + "\n" +
    "y Curves " + yCurves.toList.mkString("; ")
}

object Surface{
  def apply(points : (Double, Double, Double)*) = new Surface(points)
}

object Curve{
  def apply(points : (Double, Double)*) = new Curve(TreeMap[Double, Double]() ++ points.toMap)
  def extrapolate(xCurve : Curve, yCurve : Curve, point : (Double, Double)) : Double = {
    val (x, y) = point
    val xDist = xCurve.distanceFromEnd(x)
    val yDist = yCurve.distanceFromEnd(y)
    val z_x = xCurve(x)
    val z_y = yCurve(y)
    (z_x * yDist + z_y * xDist) / (xDist + yDist)
  }
}
