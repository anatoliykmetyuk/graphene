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
  def acpParallelism[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = product(graphs).rule(cartesian(graphs))

  /** `&` in SubScript sense. */
  def weakAnd[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = product(graphs)
    .rule(cartesian(graphs)) // Weak And is a cartesian product
    
    // SUCCESS: It has success only if all the operands have success
    .rule {case (n @ HyperNode(coords), hg, edges) if
        coords.zip(graphs).forall {case (c, g) => g.is(SUCCESS, c)}  // All the operands have success
    =>  edges ++ Set(n ~ hg.endpoints(SUCCESS))
    }

    // FAILURE: If some of the operands failed, it is a failure. However, all the operands must be in their final state - success or failure.
    .rule {case (n @ HyperNode(coords), hg, edges) if
        coords.zip(graphs).exists {case (c, g) => g.is(FAILURE, c)                    }  // At least one operand have failed
    &&  coords.zip(graphs).forall {case (c, g) => g.is(SUCCESS, c) || g.is(FAILURE, c)}  // And all the operands are either at Success or at Failure
    =>  edges ++ Set(n ~ hg.endpoints(FAILURE))
    }

  /** `|` in SubScript sense. */
  def weakOr[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = product(graphs)
    .rule(cartesian(graphs))

    // FAILURE: when all of its operands failed
    .rule {case (n @ HyperNode(coords), hg, edges) if
        coords.zip(graphs).forall {case (c, g) => g.is(FAILURE, c)}
    =>  edges ++ Set(n ~ hg.endpoints(FAILURE))
    }

    // SUCCESS: when at least one of the operands succeeded, but everyone is in its final state
    .rule {case (n @ HyperNode(coords), hg, edges) if
        coords.zip(graphs).exists {case (c, g) => g.is(SUCCESS, c)                    } // At least one of the operands succeeded
    &&  coords.zip(graphs).forall {case (c, g) => g.is(SUCCESS, c) || g.is(FAILURE, c)} // And everyone is in their final state
    =>  edges ++ Set(n ~ hg.endpoints(SUCCESS))
    }

  /** Like weakAnd, but you move directly to failure from the individual processes' failures. */
  def strongAnd[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = weakAnd(graphs: _*)
    .rule {case (n @ HyperNode(coords), hg, _) if
        coords.zip(graphs).exists {case (c, g) => g.is(FAILURE, c)}  // If at least one operand is in its failure state
    =>  Set(n ~ hg.endpoints(FAILURE))  // The only way for it is directly to global failure. No `edges ++ Set(...)`, just `Set(...)`, all previous connections are erased
    }

  /** Like weakOr, but you move directly to success from the individual processes' successes. */
  def strongOr[N](graphs: Graph[N]*): Graph[HyperNodeTrait[N]] = weakOr(graphs: _*)
    .rule {case (n @ HyperNode(coords), hg, _) if
        coords.zip(graphs).exists {case (c, g) => g.is(SUCCESS, c)}  // At least one have succeeded
    =>  Set(n ~ hg.endpoints(SUCCESS))
    }

  /** `/` in SubScript */
  def disruption[N](g1: Graph[N], g2: Graph[N]): Graph[HyperNodeTrait[N]] = product(Seq(g1, g2))
    // All the nodes are connected on `g2` axis
    .rule {case (n @ HyperNode(Seq(g1c, g2c)), hg, edges) =>
      edges ++ g2(g2c).map {g2edge => n ~ HyperNode(Seq(g1c, g2edge.destination))}
    }

    // Only [_, 0] hypernodes are connected on `g1` axis ("0" is the head of `g2`)
    .rule {case (n @ HyperNode(Seq(g1c, g2c)), hg, edges) if g2.is(HEAD, g2c) =>
      edges ++ g1(g1c).map {g1edge => n ~ HyperNode(Seq(g1edge.destination, g2c))}
    }

    // SUCCESS: Either g1 success or g2 success
    .rule {case (n @ HyperNode(Seq(g1c, g2c)), hg, edges) if
        g1.is(SUCCESS, g1c)  // g1 success
    ||  g2.is(SUCCESS, g2c)  // g2 success
    =>  Set(n ~ hg.endpoints(SUCCESS))
    }

    // FAILURE: Either g1 or g2 failure
    .rule {case (n @ HyperNode(Seq(g1c, g2c)), hg, edges) if
        g1.is(FAILURE, g1c)
    ||  g2.is(FAILURE, g2c)
    =>  Set(n ~ hg.endpoints(FAILURE))
    }

  /** Like disruption, but all the nodes [_, 1] or [_, -1] are connected on the `g1` axis also (1 is success, -1 is failure) */
  def interruption[N](g1: Graph[N], g2: Graph[N]): Graph[HyperNodeTrait[N]] = disruption(g1, g2)
    .rule {case (n @ HyperNode(Seq(g1c, g2c)), hg, edges) if g2.is(FAILURE, g2c) || g2.is(SUCCESS, g2c) =>
      g1(g1c).map {g1edge => n ~ HyperNode(Seq(g1edge.destination, g2c))}  // Don't do `edges ++` - these end nodes are no longer connected to SUCCESS and FAILURE
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

  /** Nodes of this graph are a product of the set of nodes of `graphs`. */
  def product[N](graphs: Seq[Graph[N]]): Graph[HyperNodeTrait[N]] = Graph[HyperNodeTrait[N]](
    endpoints = Map[String, HyperNodeTrait[N]](
      HEAD    -> new HyperNodeEndpoint(HEAD   )
    , SUCCESS -> new HyperNodeEndpoint(SUCCESS)
    , FAILURE -> new HyperNodeEndpoint(FAILURE)
    )
  , rules = Nil
  )
  .rule {case (n, hg, edges) if hg.is(HEAD, n) =>  // Connect HEAD to the all processes' head - [0, 0, ..., 0], where "0" is head of a corresponding graph
    edges ++ Set(n ~ HyperNode(graphs.map(_.endpoints(HEAD))))
  }

}