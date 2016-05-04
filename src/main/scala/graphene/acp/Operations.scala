package graphene.acp

import graphene.core._
import graphene.core.Graph._

object Ops extends OpsHelpers {

  /** Sequences several graphs. The result is a new graph so that the successes of the `graphs` are connected to the heads of the subsequent graphs. */
  def seq[N](graphs: Graph[N]*): Graph[N] = graphs.reduceLeft {(g1, g2) =>
    Graph(
      endpoints = Map(
        HEAD    -> g1.endpoints(HEAD   )  // Head is reused from g1
      , SUCCESS -> g2.endpoints(SUCCESS)  // Success is reused from g2, since g1.success is connected to g2.head
      )
    , g1.rules ++ g2.rules  // Union
    )
    .rule {case (n, _, edges) if n == g1.endpoints(SUCCESS) => edges ++ Set(n ~ g2.endpoints(HEAD))}  // Connect g1.success and g2.head
  }

  // `||` in ACP 1983 paper sense. A cartesian product of the graphs
  def acpParallelism[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = hypergraph[N](graphs)
    .rule(cartesian(graphs))

  /** `&` in SubScript sense. */
  def weakAnd[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = hypergraph[N](graphs)
    .rule(cartesian(graphs))          // Weak And is a cartesian product
    .rule {                           // It has success only if all the operands have success
      case (n @ HyperNode(coords), hg, edges) if coords.zip(graphs).forall {case (c, g) => g.is(SUCCESS, c)} =>
        edges ++ Set(n ~ hg.endpoints(SUCCESS))
    }
    .rule {  // If some of the operands failed, it is a failure
      case (n @ HyperNode(coords), hg, edges) if coords.zip(graphs).exists {case (c, g) => g.is(FAILURE, c)} =>
        edges ++ Set(n ~ hg.endpoints(FAILURE))
    }

  /** Like weakAnd, but you move directly to failure from the individual processes' failures. */
  def strongAnd[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = weakAnd(graphs: _*)
    .rule {case (n @ HyperNode(coords), hg, _) if coords.zip(graphs).exists {case (c, g) => g.is(FAILURE, c)} =>  // For all individual processes' failures
      Set(n ~ hg.endpoints(FAILURE))  // The only way is directly to global failure. No `edges ++ Set(...)`, just `Set(...)`, all previous connections are erased
    }

}

trait OpsHelpers {

  // Wikipedia about the cartesian product of two graphs G and H:
  //   any two vertices (u,u') and (v,v') are adjacent in G \square H if and only if either
  //     u = v and u' is adjacent with v' in H, or
  //     u' = v' and u is adjacent with v in G.
  //
  // If a hypernode [a, b, c, ..., z] was got as a result of a product of [A, B, C, ..., Z] graphs,
  // and if `a` is connected under its graph `A` to [a1, a2, a3, ... aN]
  // then this hypernode will be connected to [[a1, b, c, ...], [a2, b, c, ...], ..., [aN, b, c]].
  def cartesian[N](graphs: Seq[Graph[N]]): PartialFunction[(HyperNodeTrait[N], Graph[HyperNodeTrait[N]], Set[Edge[HyperNodeTrait[N]]]), Set[Edge[HyperNodeTrait[N]]]] = {
    case (n @ HyperNode(coords), _, edges) => edges ++ coords.zip(graphs).zipWithIndex.flatMap {case ((a, aGraph), id) =>
      val aCoords = aGraph(a)  // Get the nodes `a` connected to under `A`
      aCoords.map(aEdge => n ~ HyperNode(coords.updated(id, aEdge.destination)))
    }.toSet
  }

  def hypergraph[N](graphs: Seq[Graph[N]]): Graph[HyperNodeTrait[N]] = Graph[HyperNodeTrait[N]](
    endpoints = Map[String, HyperNodeTrait[N]](
      HEAD    -> new HyperNodeEndpoint(HEAD   )
    , SUCCESS -> new HyperNodeEndpoint(SUCCESS)
    , FAILURE -> new HyperNodeEndpoint(FAILURE)
    )
  , rules = Nil
  )
  .rule {case (n @ HyperNode(coords), hg, edges) if coords.zip(graphs).forall {case (c, g) => g.is(HEAD, c)} => edges ++ Set(hg.endpoints(HEAD) ~ n)}  // Connect HEAD to the all processes' head

}