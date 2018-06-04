case class Node(i: Int)

class DirectedGraph(val new_edges: List[(Node,Node)]) {

  val edges: List[(Node,Node)] = new_edges

  def addEdge(edge: (Node,Node)) = new DirectedGraph(edge :: edges)
}

object graph {
	
 	def main(args: Array[String]) {

		//Very simple graph
		val graph = new DirectedGraph(List((Node(1), Node(2)), (Node(2), Node(3)), (Node(1), Node(4))))

		//TODO: find all vertices
		println(graph.edges.flatMap(t => List(t._1, t._2)).distinct)

		//TODO: find all source and sink vertices of the graph
		println(graph.edges.filter { case (v, x) => v != graph.edges. })

		//TODO: get maximum outgoing graph degree
	}
}
