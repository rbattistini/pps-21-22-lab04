package u04lab.polyglot

trait Tup2[A, B]:
  def x: A
  def y: B

object Tup2:
  def apply[A, B](x: A, y: B) : Tup2[A, B] = Tup2Impl[A, B](x, y)
  def unapply[A, B](t: Tup2[A, B]): Option[(A, B)] = Some((t.x, t.y))
  private case class Tup2Impl[A, B](override val x: A,
                                    override val y: B) extends Tup2[A, B]