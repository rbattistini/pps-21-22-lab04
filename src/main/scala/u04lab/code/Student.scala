package u04lab.code

import List.*
import List.{append, contains}
import Option.isEmpty

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit
  def courses: List[String]
  def hasTeacher(teacher: String): Boolean

trait Course:
  def name: String
  def teacher: String

object Student:
  def apply(name: String, year: Int = 2017): Student =
    StudentImpl(name, year)
  private case class StudentImpl(override val name: String,
                                 override val year: Int) extends Student:
    private var coursesList: List[Course] = Nil()

    override def enrolling(course: Course*): Unit =
      for c <- course do coursesList = append(Cons(c, Nil()), coursesList)

    override def courses: List[String] = map(coursesList)(_.name)

    override def hasTeacher(teacher: String): Boolean =
      contains(map(coursesList)(_.teacher), teacher)

object Course:
  def apply(name: String, teacher: String): Course =
    CourseImpl(name, teacher)
  private case class CourseImpl(override val name: String,
                                override val teacher: String) extends Course

object SameTeacher:
  def unapply(courses: List[Course]): scala.Option[String] =
    val teachers = map(courses)(_.teacher)
    teachers match
      case Cons(h, t) if isEmpty(find(teachers)(_ != h)) => scala.Option(h)
      case _ => scala.Option.empty
