package compiler
import AReg._

// this layer of abstraction is used so we can lower this in tests, to more easily see what's going on
// otherwise, it should always be set to 11
case class KAllocationBound(k: Int) { 1 <= k && k <= 11 }
given KAllocationBound = KAllocationBound(11)

val registerColorCorrespondence: Map[Int, AReg] = Map(
  -1 -> RAX,
  -2 -> RSP,
  -3 -> RBP,
  -4 -> R11,
  -5 -> R15,
  0 -> RCX,
  1 -> RDX,
  2 -> RSI,
  3 -> RDI,
  4 -> R8,
  5 -> R9,
  6 -> R10,
  7 -> RBX,
  8 -> R12,
  9 -> R13,
  10 -> R14
)

extension (arg: AsmArg)
  def isVar: Boolean = arg match
    case VarAsmArg(_) => true
    case _            => false

def allocateRegisters(program: Ax86Program)(using KAllocationBound): Ax86Program = {
  assert(registerColorCorrespondence.size == AReg.values.size)
  assert(registerColorCorrespondence.size == registerColorCorrespondence.values.toSet.size)

  val graph = program.info("conflicts").asInstanceOf[UndirectedGraph[AsmArg]]

  val todoVariables: collection.mutable.Set[AsmArg] = graph.vertices.filter(_.isVar).to(collection.mutable.Set)
  val colors = collection.mutable.Map[AsmArg, Some[Int]]()
  for (k, v) <- registerColorCorrespondence do
    colors(RegAsmArg(v)) = Some(k)
  val restrictions = collection.mutable.Map[AsmArg, collection.mutable.Set[Int]]()
  for v <- graph.vertices do
    restrictions(v) = collection.mutable.Set()
    for w <- graph.edges(v); if w != v do
      if colors.contains(w) then restrictions(v) += colors(w).get

  while todoVariables.nonEmpty do
    val varPicked = todoVariables.map(vr => (-restrictions(vr).size, vr)).toVector.sortBy(_._1).head._2
    var pickedColor = -1
    var consideredColor = 0
    while pickedColor == -1 do
      if !restrictions(varPicked).contains(consideredColor) then
        pickedColor = consideredColor
        todoVariables -= varPicked
        colors(varPicked) = Some(pickedColor)
        for w <- graph.edges(varPicked) do
          restrictions(w) += pickedColor
      else
        consideredColor += 1

  assert(program.blocks.size == 1 && program.blocks.contains("start"))
  val startBlock = program.blocks("start")

  var largestStackPositionSpilled = 0 // 0 means no
  var `callee-saved register we had used` = Set[AReg]()
  def asmArgToLocation
    (arg: AsmArg, colors: collection.mutable.Map[AsmArg, Some[Int]])
    (using k: KAllocationBound)
    : AsmArg =
    arg match
      case VarAsmArg(_) => {
        if colors.contains(arg) then
          val color = colors(arg).get
          assert(0 <= color)
          if registerColorCorrespondence.contains(color) && color < k.k then
            val regHere = registerColorCorrespondence(color)
            if regHere.isCalleeSaved then
              `callee-saved register we had used` += regHere
            RegAsmArg(regHere)
          else
            assert(k.k <= color)
            largestStackPositionSpilled = largestStackPositionSpilled.max(color - k.k + 1)
            DerefAsmArg(AReg.RBP, -8 * (color - k.k + 1))
        else
          arg
      }
      case _ => arg

  val newStartBlock = ABlock(
    startBlock.info,
    startBlock.body.map { instr =>
      instr match
        case AMovq(src, dst) => AMovq(asmArgToLocation(src, colors), asmArgToLocation(dst, colors))
        case AAddq(src, dst) => AAddq(asmArgToLocation(src, colors), asmArgToLocation(dst, colors))
        case ASubq(src, dst) => ASubq(asmArgToLocation(src, colors), asmArgToLocation(dst, colors))
        case ANegq(dst)      => ANegq(asmArgToLocation(dst, colors))
        case APushq(src)     => APushq(asmArgToLocation(src, colors))
        case APopq(dst)      => APopq(asmArgToLocation(dst, colors))
        case _               => instr
    }
  )
  Ax86Program(
    program.info ++ Map(
      "spilled-fields" -> largestStackPositionSpilled,
      "used-callee" -> `callee-saved register we had used`
    ),
    Map("start" -> newStartBlock)
  )
}
