package u04lab.polyglot.a05b
//class Pair[A, B](val x: A, val y: B)
//class MyPair(override val x: Double, override val y: Double) extends Pair(x,y)
//
//object Pair:
//  def apply[A, B](x: A, y: B) = new Pair(x, y)
//  def unapply[A, B](p: Pair[A, B]): Option[(A, B)] = Some((p.x, p.y))

import scala.util.Random

trait Tup2[A, B]:
  def x: A
  def y: B

object Tup2:
  def apply[A, B](x: A, y: B) : Tup2[A, B] = Tup2Impl[A, B](x, y)
  def unapply[A, B](t: Tup2[A, B]): Option[(A, B)] = Some((t.x, t.y))
  private case class Tup2Impl[A, B](override val x: A,
                                    override val y: B) extends Tup2[A, B]

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val gridSize: Int) extends Logics:

  private val random: Random = Random()
  private val initial: Tup2[Int, Int] =
    Tup2(random.nextInt(gridSize - 2) + 1, random.nextInt(gridSize - 2) + 1)
  private var tickCount = 0
  
  def isTupleAnElement(t1: Tup2[Int, Int], t2: Tup2[Int, Int], offset: Int): Boolean = (t1, t2) match
    case (t1, t2) if t1.x == t2.x && math.abs(t1.y - t2.y) <= offset |
      t1.y == t2.y && math.abs(t1.x - t2.x) <= offset |
      t1.x - t1.y == t2.x - t2.y && math.abs(t1.x - t2.x) <= offset |
      t1.x + t1.y == t2.x + t2.y && math.abs(t1.x - t2.x) <= offset => true
    case _ => false

  def isTupleOutsideGrid(t: Tup2[Int, Int], offset: Int, gridSize: Int): Boolean = t match
    case t if t.y - offset < 0 |
      t.y + offset >= gridSize |
      t.x - offset < 0 |
      t.x + offset > gridSize => true
    case _ => false

  override def tick(): Unit = {tickCount = tickCount + 1}

  override def isOver: Boolean = initial match
    case initial if isTupleOutsideGrid(initial, tickCount, gridSize) => true
    case _ => false

  override def hasElement(x: Int, y: Int): Boolean = (x, y) match
    case (x, y) if isTupleAnElement(Tup2(x, y), initial, tickCount) => true
    case _ => false
