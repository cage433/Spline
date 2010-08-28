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
}

class Surface(points : List[Triple]){
  lazy val (xCurves, yCurves) = {
    var xCurves = TreeMap[Double, Curve]()
    var yCurves = TreeMap[Double, Curve]()
    points.foreach{
      case Triple(x, y, z) =>
        xCurves += x -> (xCurves.getOrElse(x, Curve()) + (y, z))
        yCurves += y -> (yCurves.getOrElse(y, Curve()) + (x, z))
    }
    (xCurves, yCurves)
  }
}
object Curve{
  def apply(points : (Double, Double)*) = new Curve(TreeMap[Double, Double]() ++ points.toMap)
}
