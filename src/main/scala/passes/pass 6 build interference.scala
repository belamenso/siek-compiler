package compiler

class UndirectedGraph[T] {
  var vertices: Set[T] = Set()
  var edges: Map[T, Set[T]] = Map()

  def addVertex(v: T): Unit =
    if !vertices.contains(v) then
      vertices += v
      edges += (v -> Set())

  def addEdge(v1: T, v2: T): Unit =
    addVertex(v1)
    addVertex(v2)
    if !edges(v1).contains(v2) then
      edges += (v1 -> (edges(v1) + v2))
    if !edges(v2).contains(v1) then
      edges += (v2 -> (edges(v2) + v1))
}

def buildInterference(program: Ax86Program): Ax86Program = {
  assert(program.blocks.size == 1 && program.blocks.contains("start"))
  val onlyBlock = program.blocks("start")
  val liveAfterSets = onlyBlock.info("live-after").asInstanceOf[List[Set[AsmArg]]].tail
  assert(onlyBlock.body.size == liveAfterSets.size)

  val graph = new UndirectedGraph[AsmArg]

  for ((instr, liveAfter) <- onlyBlock.body.zip(liveAfterSets)) do
    instr match {
      case AMovq(src, dst) =>
        for (v <- liveAfter; if v != src && v != dst) do
          graph.addEdge(dst, v)
      case _ =>
        for (d <- instr.writtenLocations; v <- liveAfter; if v != d) do
          graph.addEdge(d, v)
    }

  pprint.pprintln(graph.edges)

  Ax86Program(
    program.info + ("conflicts" -> graph),
    program.blocks
  )
}
