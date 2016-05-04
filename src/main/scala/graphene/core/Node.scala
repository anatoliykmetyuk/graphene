package graphene.core

case class Edge[N](source: N, destination: N)

/** A node got as a result of a product of two or more Graph[N]. */
trait HyperNodeTrait[N] {
  def coordinates: Seq[N]
}

case class HyperNode[N](coordinates: Seq[N]) extends HyperNodeTrait[N] {
  override def toString = "[" + coordinates.mkString(", ") + "]"
}

/** Used to specify endpoints for hypergraphs */
class HyperNodeEndpoint[N](name: String) extends HyperNodeTrait[N] {
  val coordinates = Nil
  override def toString = s"[$name]"
}
