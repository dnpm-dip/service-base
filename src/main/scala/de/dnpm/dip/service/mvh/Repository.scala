package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


trait Repository[F[_],Env,T <: PatientRecord]
{

  protected implicit def toPredicate(
    filter: MVHPatientRecord.Filter
  ): MVHPatientRecord[T] => Boolean =
    record =>
      filter.submissionPeriod
        .map(_ contains record.submittedAt)
        .getOrElse(true)

  def save(
    record: MVHPatientRecord[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]]

  def ?(filter: MVHPatientRecord.Filter)(
    implicit env: Env
  ): F[Iterable[MVHPatientRecord[T]]]

  def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]]

}

