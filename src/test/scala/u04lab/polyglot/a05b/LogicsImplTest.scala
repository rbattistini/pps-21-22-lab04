package u04lab.polyglot.a05b

import org.junit.*
import org.junit.Assert.*
import u04lab.polyglot.a05b.Tup2

class LogicsImplTest:
  private val t1 = Tup2(2, 2)
  private val t2 = Tup2(4, 2)
  private val gridSize = 4
  private val logicsImpl = LogicsImpl(gridSize)

  @Test def testExitConditionTrue(): Unit =
    val tickCount = 2
    assertTrue(logicsImpl.isTupleOutsideGrid(t2, tickCount, gridSize))

  @Test def testExitConditionFalse(): Unit =
    val tickCount = 1
    assertFalse(logicsImpl.isTupleOutsideGrid(t1, tickCount, gridSize))

  @Test def testHasElementTrue(): Unit =
    val tickCount = 2
    assertTrue(logicsImpl.isTupleAnElement(t1, t2, tickCount))

  @Test def testHasElementFalse(): Unit =
    val tickCount = 1
    assertFalse(logicsImpl.isTupleAnElement(t1, t2, tickCount))