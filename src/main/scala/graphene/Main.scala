package graphene

import graphene.core._
import graphene.util._
import graphene.acp._

import graphene.core.Graph._
import graphene.acp .Ops._
import graphene.util.IO._
import graphene.util.DummyGraphs._


object Main {
  def main(args: Array[String]) {
    // Basic sequences we will be working with
    val Seq(s1, s2) = seqs.take(2)

    // Save graph to workdir as GEXF
    def write(g: Graph[_], filename: String) {g serialize file(filename + ".gexf")}

    // Visualize them as a graph
    write(s1, "primitive")

    // ACP paper parallelism
    write(acpParallelism(s1, s2), "acpParallelism")


    // === SubScript Operators ===
    write(weakAnd  (s1, s2), "weakAnd"  )  // &
    write(weakOr   (s1, s2), "weakOr"   )  // |
    write(strongAnd(s1, s2), "strongAnd")  // &&
    write(strongOr (s1, s2), "strongOr" )  // ||

    write(disruption  (s1, s2), "disruption"  ) // `/`
    write(interruption(s1, s2), "interruption")

    println("Sample graphs created in the `workdir` directory")


    // === Handling exponentialy growing amount of nodes ===
    val g     = acpParallelism(seqs.take(10): _*)  // ~5^10 == ~9 750 000 nodes
    val head  = g.endpoints(HEAD)
    val child = g(head).toSeq.head.destination

    println(s"Head of the large graph: $head")
    println(s"Some child of the large graph's head: $child")
    println(s"Rules set describing the large graph: ${g.rules.size}")
    println(s"Endpoints of the large graph: ${g.endpoints}")
  }
}