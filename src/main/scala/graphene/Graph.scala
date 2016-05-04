package graphene

import scala.collection.mutable
import Graph._

case class Edge[N](source: N, destination: N)

case class Rule[N](rule: PartialFunction[N, Set[Edge[N]]], addition: Boolean)

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


class Graph[N](val endpoints: Endpoints[N], val rules: List[Rule[N]]) extends Function1[N, Set[Edge[N]]] {
  def connect   (r: PartialFunction[N, Set[Edge[N]]]) = Graph(endpoints, rules :+ Rule(r, true ))
  def disconnect(r: PartialFunction[N, Set[Edge[N]]]) = Graph(endpoints, rules :+ Rule(r, false))

  override def apply(n: N): Set[Edge[N]] = {
    def extractEdges(addition: Boolean): Set[Edge[N]] =
      rules.filter(_.addition == addition).map(_.rule).filter(_.isDefinedAt(n)).flatMap(_(n)).toSet
    
    extractEdges(true) -- extractEdges(false)
  }

  /** Nodes connected to the `head` - directly or indirectly. */
  def nodes: Set[N] = edges.flatMap(e => Set(e.source, e.destination))

  /** Edges of the nodes connected to `head`. */
  def edges: Set[Edge[N]] = {
    def collect(n: N): Set[Edge[N]] = {
      val ex = apply(n)
      ex ++ ex.flatMap(e => collect(e.destination))
    }
    collect(endpoints(HEAD))
  }

  /** Tests whether the given node is a given endpoint. */
  def is(name: String, node: N): Boolean =
    endpoints.get(name).map(_ == node).getOrElse(false)

  def toXml: xml.Node = {
    def genColor(node: N, endpointName: String, color: (Int, Int, Int)) =
      {if (endpoints.get(endpointName).map(_ == node).getOrElse(false)) <viz:color r={color._1.toString} g={color._2.toString} b={color._3.toString} a="0.6"/>}

    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.1draft/viz" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      <graph defaultedgetype="directed">
        <nodes>
          {nodes.map {n =>
            <node id={n.hashCode.toString} label={n.toString}>51, 204, 51
              {genColor(n, HEAD   , (230, 230, 0 ))}  // Head is Yellow
              {genColor(n, SUCCESS, (51 , 204, 51))}  // Success is Green
              {genColor(n, FAILURE, (255, 51 , 0 ))}  // Failure is Red
            </node>
          }}
        </nodes>
        <edges>
          {edges.map {case e @ Edge(source, destination) =>
            <edge id={e.hashCode.toString} source={source.hashCode.toString} target={destination.hashCode.toString}/>
          }}
        </edges>
      </graph>
    </gexf>
  }

  def serialize(file: java.io.File) = IO.save(toXml.toString, file)
}

object Graph {
  type Endpoints   [N] = Map[String, N]
  type RuleFunction[N] = PartialFunction[N, Set[N]]

  val HEAD    = "head"
  val SUCCESS = "success"
  val FAILURE = "failure"

  implicit class EdgeBuilder[N](n: N) {
    def ~(n2: N): Edge[N] = Edge(n, n2)
  }

  def apply[N](endpoints: Endpoints[N], rules: List[Rule[N]]) = new Graph(endpoints, rules)

  def apply[N](endpoints: Endpoints[N], edges: Edge[N]*): Graph[N] = {
    // Writing the rules for the edges
    val g = edges.foldLeft(new Graph[N](endpoints, Nil))
      {(g, e) => g.connect {case e.source => Set(e)}}

    // Connecting the graph entry point to the first edge's source
    g.connect {case n if n == g.endpoints(HEAD) => Set(g.endpoints(HEAD) ~ edges.head.source)}
  }
}

object DummyGraphs {
  import Graph._

  case class Counter(start: Int, increment: Int = -1) {
    private[this] var counter = start
    def nextInt: Int = {counter += increment; counter}
  }

  // No negative Int nodes in the graphs the counters' values are to be used in
  // No more then 1000 calls to nextInt before an overlap between the counters happens
  val head    = Counter(-1000)  // All the heads have "1" at their start
  val success = Counter(-2000)  // Successes begin with "2"
  val failure = Counter(-3000)  // Obvious thing

  // Unique endpoints for each grah
  def intEndpoints: Endpoints[Int] = Map(
    HEAD     -> head   .nextInt
  , SUCCESS  -> success.nextInt
  , FAILURE  -> failure.nextInt
  )

  // Sequence of Int nodes from start to end
  def sequence(start: Int, end: Int) = {
    lazy val g: Graph[Int] = Graph(
      intEndpoints
    , (start to end).sliding(2).map {window => window.head ~ window.last}.toList: _*
    )
    .connect {case n if n == end => Set(n ~ g.endpoints(SUCCESS))} // Success is after the final node
    g
  }

  // An ordinary sequence with the Failure node available - to simulate different operators where Failure matters
  def sequneceWithFailure(start: Int, end: Int) = {
    lazy val g: Graph[Int] = sequence(start, end)
      .connect {case n if n == end => Set(n ~ g.endpoints(FAILURE))} // Dummy failure implementation as another child of the last node of the sequence
    g
  }
}