package u04lab.code

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import u04lab.code.List.{Cons, Nil}
import u04lab.code.{Course, List, SameTeacher, Student}

class StudentTest:
  private val cPPS = Course("PPS", "Viroli")
  private val cPCD = Course("PCD", "Ricci")
  private val cSDR = Course("SDR", "D'Angelo")
  private val cPER = Course("Pervasive", "Ricci")
  private val s1 = Student("mario", 2015)
  private val s2 = Student("rino")

  def testSameTeacher(c: List[Course]): Boolean = c match
    case SameTeacher(s) => true
    case _ => false

  @Test def testEnrolling(): Unit =
    s2.enrolling(cPPS, cPCD, cSDR)
    assertEquals(s2.courses, Cons("SDR", Cons("PCD", Cons("PPS", Nil()))))

  @Test def testHasTeacher(): Unit =
    s1.enrolling(cPPS)
    assertTrue(s1.hasTeacher("Viroli"))

  @Test def testSameTeacherFalse(): Unit =
    assertFalse(testSameTeacher(Cons(cPPS, Cons(cPCD, Cons(cSDR, Nil())))))

  @Test def testSameTeacherTrue(): Unit =
    assertTrue(testSameTeacher(Cons(cPCD, Cons(cPER, Nil()))))
