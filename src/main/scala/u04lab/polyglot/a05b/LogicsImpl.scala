package u04lab.polyglot.a05b

import scala.util.Random

final case class Tup2[A, B](x: A, y: B)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val gridSize: Int) extends Logics:

  private val random: Random = Random()
  private val initial: Tup2[Int, Int] =
    Tup2(random.nextInt(gridSize - 2) + 1, random.nextInt(gridSize - 2) + 1)
  private var tickCount = 0

  private def onSameAxis(x: Int, y: Int): Boolean = x == y
  private def isInRange(x: Int, y: Int, offset: Int): Boolean = math.abs(x -y) <= offset

  private def onXAxis(t1: Tup2[Int, Int], t2: Tup2[Int, Int], offset: Int): Boolean =
    onSameAxis(t1.x, t2.x) && isInRange(t1.y, t2.y, offset)
  private def onYAxis(t1: Tup2[Int, Int], t2: Tup2[Int, Int], offset: Int): Boolean =
    onSameAxis(t1.y, t2.y) && isInRange(t1.x, t2.x, offset)
  private def onDiagonal(t1: Tup2[Int, Int], t2: Tup2[Int, Int], offset: Int): Boolean =
    t1.x - t1.y == t2.x - t2.y && isInRange(t1.x, t2.x, offset) ||
    t1.x + t1.y == t2.x + t2.y && isInRange(t1.x, t2.x, offset)

  def isTupleAnElement(t1: Tup2[Int, Int], t2: Tup2[Int, Int], offset: Int): Boolean = (t1, t2) match
    case (t1, t2) if onXAxis(t1, t2, offset) || onYAxis(t1, t2, offset) || onDiagonal(t1, t2, offset) => true
    case _ => false

  def isTupleOutsideGrid(t: Tup2[Int, Int], offset: Int, gridSize: Int): Boolean = t match
    case t if t.y - offset < 0 || t.y + offset >= gridSize || t.x - offset < 0 || t.x + offset > gridSize => true
    case _ => false

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean = initial match
    case initial if isTupleOutsideGrid(initial, tickCount, gridSize) => true
    case _ => false

  override def hasElement(x: Int, y: Int): Boolean = (x, y) match
    case (x, y) if isTupleAnElement(Tup2(x, y), initial, tickCount) => true
    case _ => false
