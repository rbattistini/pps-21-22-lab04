package u04lab.code

import org.junit.Assert.assertEquals
import org.junit.Test
import u04lab.code.List
import u04lab.code.List.*

class ListTest:
  val l: List[Int] = List(2, 4, 6)

  @Test def testCreation(): Unit = assertEquals(l, Cons(2, Cons(4, Cons(6, Nil()))))

