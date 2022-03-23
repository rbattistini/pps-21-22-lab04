package u04lab.code

import org.junit.Assert.assertEquals
import org.junit.Test
import u04lab.code.Complex

class ComplexTest:
  private val a = Array(Complex(10, 20), Complex(1, 1), Complex(7, 0))

  @Test def testSum(): Unit = assertEquals(a(0) + a(1) + a(2), Complex(18.0, 21.0))

  @Test def testMultiplication(): Unit = assertEquals(a(0) * a(1), Complex(-10.0, 30.0))

