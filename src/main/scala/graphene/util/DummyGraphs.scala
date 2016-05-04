package graphene.util

import graphene.core._

/** Utils to quickly create graphs */
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
  def sequence(start: Int, end: Int) = Graph(
      intEndpoints
    , (start to end).sliding(2).map {window => window.head ~ window.last}.toList: _*
    )
    .rule {case (n, g, edges) if n == end => edges ++ Set(n ~ g.endpoints(SUCCESS))} // Success is after the final node


  /** An ordinary sequence with the Failure node available - to simulate different operators where Failure matters */
  def sequneceWithFailure(start: Int, end: Int) = sequence(start, end)
    .rule {case (n, g, edges) if n == end => edges ++ Set(n ~ g.endpoints(FAILURE))} // Dummy failure implementation as another child of the last node of the sequence
}