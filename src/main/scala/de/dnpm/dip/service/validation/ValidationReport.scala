package de.dnpm.dip.service.validation


import java.time.Instant
import cats.data.NonEmptyList
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  Writes,
  JsString
}


case class Issue
(
  severity: Issue.Severity.Value,
  message: String,
  path: Issue.Path
)

object Issue
{

  object Severity extends Enumeration
  {
    val Info    = Value("info")
    val Warning = Value("warning")
    val Error   = Value("error")
    val Fatal   = Value("fatal")
  
    implicit val format: Format[Severity.Value] =
      Json.formatEnum(this)
  }
  

  case class Path(nodes: List[String])
  {
    def /(node: String): Path =
      copy(nodes = nodes :+ node)

    import scala.language.reflectiveCalls

    def /[T](t: T)(
      implicit
      node: Path.Node[T],
      hasId: T <:< { def id: Id[_] }
//      hasId: T <:< { def id: Id[T] }
    ): Path =
      this/s"${node.name}[${t.id.value}]"

//    def /[T](t: T)(implicit node: Path.Node[T]): Path =
//      this/node.name

    override def toString: String =
      s"/${nodes.mkString("/")}"

  }

  object Path
  {

    @annotation.implicitNotFound("Couldn't find Path.Node[${T}] instance. Define one or ensure it is in implicit scope.")
    sealed trait Node[T]{ val name: String }
    object Node
    {
      def apply[T](implicit node: Node[T]) = node    

      def apply[T](n: String): Node[T] =
        new Node[T]{ val name = n }
    }

    val root: Path = Path(Nil)

    def from(str: String): Path =
      Path(str.split("/").toList)

    implicit val reads: Reads[Path] =
      Reads(js => js.validate[String].map(Path.from))

    implicit val writes: Writes[Path] =
      Writes(p => JsString(p.toString))
  }


  sealed trait Builder
  {
    def at(path: Path): Issue
  }   

  private case class BuilderImpl
  (
    sev: Severity.Value,
    msg: String
  )
  extends Builder
  {
    def at(path: Path): Issue = Issue(sev,msg,path)
  }


  def Info(msg: String): Builder =
    BuilderImpl(Severity.Info,msg)

  def Warning(msg: String): Builder =
    BuilderImpl(Severity.Warning,msg)

  def Error(msg: String): Builder =
    BuilderImpl(Severity.Error,msg)

  def Fatal(msg: String): Builder =
    BuilderImpl(Severity.Fatal,msg)

  
  implicit val format: Format[Issue] =
    Json.format[Issue]

}

case class ValidationReport
(
  patient: Id[Patient],
  issues: NonEmptyList[Issue],
  createdAt: Instant
)
{
  val maxSeverity =
    issues.toList.map(_.severity).max
}


object ValidationReport
{

  def apply(
    patient: Id[Patient],
    issues: NonEmptyList[Issue]
  ): ValidationReport =
    ValidationReport(patient,issues,Instant.now)


  // For format[NonEmptyList[_]]
  import de.dnpm.dip.util.json._

  implicit val format: Format[ValidationReport] = 
    Json.format[ValidationReport]

}
