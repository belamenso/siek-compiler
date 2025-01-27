import compiler._

class GenerateAsm extends munit.FunSuite {
  test("generate asm 1") {
    val p = """(let [a 1] (let [b a] b))"""
    val x = (uniquify(readProgram(p)))
    val x1 = removeComplexOperands(x)
    val y = explicateControl(x1)
    val y1 = selectInstructions(y)
    val z = patchInstructions(assignHomes(y1))
    val asm = generateAsm(z)

    val expected = """start:
  movq $1, -8(%rbp)
  movq -8(%rbp), %rax
  movq %rax, -16(%rbp)
  movq -16(%rbp), %rax
  jmp conclusion

  .globl main
main:
  pushq %rbp
  movq %rsp, %rbp
  subq $16, %rsp
  jmp start

conclusion:
  addq $16, %rsp
  popq %rbp
  retq"""

    assertEquals(asm.strip, expected.strip)
  }
}
