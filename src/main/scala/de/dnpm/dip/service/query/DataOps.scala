package de.dnpm.dip.service.query


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
}


object Data
{

  sealed abstract class Command[+T] 

  final case class Save[PatientRecord]
  (
    dataSet: PatientRecord
  )
  extends Command[PatientRecord]

  final case class Delete
  (
    patient: Id[Patient],
  )
  extends Command[Nothing]


  sealed abstract class Outcome[+T]

  final case class Saved[PatientRecord](
    snapshot: Snapshot[PatientRecord]
  )
  extends Outcome[PatientRecord]

  final case class Deleted
  (
    patient: Id[Patient], 
  )
  extends Outcome[Nothing]


  trait Ops[
    F[_],
    Env,
    UseCase <: UseCaseConfig,
    Error
  ]
  {
    self =>

    def process(
      cmd: Command[UseCase#PatientRecord]
    )(
      implicit env: Env
    ): F[Either[Error,Outcome[UseCase#PatientRecord]]]

    final def !(
      cmd: Command[UseCase#PatientRecord]
    )(
      implicit env: Env
    ): F[Either[Error,Outcome[UseCase#PatientRecord]]] = self.process(cmd)


  }

}
