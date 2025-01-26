package compiler

import scala.compiletime.ops.double

def removeComplexOperands(prog: ProgramL): ProgramL =
  var counter = 1

  def removeComplexOperandsExpr(e: LExpr): LExpr =
    e match
      case IntL(n)                 => IntL(n)
      case VarL(name)              => VarL(name)
      case LetL(name, value, body) => LetL(name, removeComplexOperandsExpr(value), removeComplexOperandsExpr(body))
      case PrimL(op, args) =>
        val (newArgs, newDefs) = args.map(removeComplexOperandsAtom).unzip
        assert(newDefs.flatten.map(_._1).size == newDefs.flatten.map(_._1).toSet.size)
        var current: LExpr = PrimL(op, newArgs)
        for
          d <- newDefs.reverse
          (name, value) <- d.reverse
        do
          current = LetL(name, value, current)
        current

  /// called on things that need to become atoms
  def removeComplexOperandsAtom(e: LExpr): (LExpr, List[(String, LExpr)]) =
    e match
      case IntL(n)    => (e, List())
      case VarL(name) => (e, List())
      case LetL(name, value, body) =>
        val newValue = removeComplexOperandsExpr(value)
        val newBody = removeComplexOperandsExpr(body)

        newBody match { // do not introduce a new let if the body is already atomic
          case VarL(_) | IntL(_) => (newBody, List(name -> newValue))
          case _ => {
            val newName = ".tmp." + counter // starts with `.` so it's a reserved name
            counter += 1
            (VarL(newName), List(newName -> LetL(name, newValue, newBody)))
          }
        }
      case PrimL(op, args) =>
        val newName = ".tmp." + counter // starts with `.` so it's a reserved name
        counter += 1
        val (newArgs, newDefs) = args.map(removeComplexOperandsAtom).unzip
        val newDef = newDefs.reduce(_ ++ _)
        (VarL(newName), newDef ++ List(newName -> PrimL(op, newArgs)))

  ProgramL(prog.info, removeComplexOperandsExpr(prog.body))
