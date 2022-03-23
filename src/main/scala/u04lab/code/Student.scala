package u04lab.code

import List.*
import List.{append, contains}
import Option.isEmpty

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course): Unit
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

    override def enrolling(course: Course): Unit =
      coursesList = append(Cons(course, Nil()), coursesList)

    override def enrolling(course: Course*): Unit =
      for c <- course do coursesList = append(Cons(c, Nil()), coursesList)

    override def courses: List[String] = map(coursesList)((c: Course) => c.name)

    override def hasTeacher(teacher: String): Boolean =
      contains(map(coursesList)((c: Course) => c.teacher), teacher)

object Course:
  def apply(name: String, teacher: String): Course =
    CourseImpl(name, teacher)
  private case class CourseImpl(override val name: String,
                                override val teacher: String) extends Course

object SameTeacher:
  def unapply(courses: List[Course]): scala.Option[String] =
    val teachers = map(courses)(_.teacher)
    teachers match
      case Cons(h, t) if isEmpty(find(teachers)((s: String) => s != h)) => scala.Option(h)
      case _ => scala.Option.empty
