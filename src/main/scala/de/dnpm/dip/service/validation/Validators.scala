package de.dnpm.dip.service.validation


import java.net.URI
import scala.util.matching.Regex
import scala.util.chaining._
import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.validated._
import de.ekut.tbi.validation.{
  CanContain,
  NegatableValidator,
  Validator
}
import de.ekut.tbi.validation.dsl._
import de.dnpm.dip.coding.{
  Code,
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.UnregisteredMedication
import de.dnpm.dip.model.{
  BaseVariant,
  ClosedInterval,
  Diagnosis,
  GeneAlterationReference,
  Id,
  Interval,
  SystemicTherapy,
  Obs,
  Patient,
  PatientRecord,
  Procedure,
  Recommendation,
  MedicationRecommendation,
  Reference,
  ExternalReference,
  Study,
  StudyEnrollmentRecommendation,
  Therapy,
  TherapyRecommendation
}
import de.dnpm.dip.service.DataUpload
import de.dnpm.dip.service.mvh.{
  BroadConsent,
  Submission
}
import de.dnpm.dip.service.mvh.ModelProjectConsent.Purpose._
import de.dnpm.dip.service.mvh.Consent.Provision.Type._
import Issue.{
  Warning,
  Error,
  Fatal,
  Path,
}
import shapeless.{
  Coproduct,
  :+:,
  CNil
}


trait Validators
{


  implicit class IssueBuilderValidatedNelExtensions[T](
    val validated: ValidatedNel[Issue.Builder,T]
  ){
    def at(path: Path): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(_ at path))

    def at(node: String): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(_ at Path.root/node))

  }


  implicit class IssueValidatedNelExtensions[T](
    val validated: ValidatedNel[Issue,T]
  ){
    def on(path: Path): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = path /: issue.path)))

    def on(node: String): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = node /: issue.path)))

    def on(t: T)(implicit hasId: HasId[T], node: Path.Node[T]): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = t /: issue.path)))
  }


  // CanContain implementation for Interval sub-types
  implicit def intervalCanContain[T, C[x] <: Interval[x]]: CanContain[T,C[T]] =
    new CanContain[T,C[T]]{

      override def contains(c: C[T])(t: T): Boolean =
        c contains t

      override def containsOnly(c: C[T])(t: T): Boolean =
        c contains t
    }



  implicit val patientNode: Path.Node[Patient] =
    Path.Node("Patient")

  implicit val baseVariantNode: Path.Node[BaseVariant] =
    Path.Node("Variante")

  implicit def diagnosisNode[D <: Diagnosis]: Path.Node[D] =
    Path.Node("Diagnose")

  implicit def therapyRecommendationNode[R <: TherapyRecommendation]: Path.Node[R] =
    Path.Node("Therapie-Empfehlung")

  implicit def studyRecommendationNode[T <: StudyEnrollmentRecommendation]: Path.Node[T] =
    Path.Node("Studien-Einschluss-Empfehlung")

  implicit def medTherapyNode[T <: SystemicTherapy[_]]: Path.Node[T] =
    Path.Node("Systemische-Therapie")

  implicit def procedureNode[P <: Procedure]: Path.Node[P] =
    Path.Node("Prozedur")


  def MissingValue(
    name: String,
    severity: Issue.Severity.Value = Issue.Severity.Warning
  ): Issue =
    Issue(
      severity,
      s"Fehlende Angabe '$name'",
      Path.root/name
    )

  def MissingOptValue(
    name: String
  ): Issue =
    Issue(
      Issue.Severity.Info,
      s"Fehlende optionale Angabe '$name', ggf. nachprüfen, ob nachzureichen",
      Path.root/name
    )

  def MissingResult(
    name: String,
    severity: Issue.Severity.Value = Issue.Severity.Warning
  ): Issue = 
    Issue(
      severity,
      s"Fehlende(r) Befund(e) '$name'",
      Path.root/name
    )

  def MissingResult[T: Path.Node]: Issue =
    MissingResult(Path.Node[T].name)



  // For implicit conversions to NegatableValidator[Issue.Builder,T]
  private implicit lazy val defaultIssueBuilder: String => Issue.Builder =
    Error(_)


  // Unregistered Medications are valid by default,
  // as they cannot be validated against a given CodeSystem
  implicit val unregisteredMedicationValidator: NegatableValidator[Issue.Builder,Coding[UnregisteredMedication]] =
    _.validNel[Issue.Builder]


  implicit def csCodingValidator[T](
    implicit cs: CodeSystem[T]
  ): NegatableValidator[Issue.Builder,Coding[T]] =
    coding =>
      cs.concept(coding.code) must be (defined) otherwise (
        Error(s"Ungültiger Code '${coding.code.value}' gemäß Kodier-System '${cs.name}'${cs.version.map(v => s" (Version '$v')").getOrElse("")}")
      ) map (_ => coding)


  implicit def cspCodingValidator[T](
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue.Builder,Coding[T]] =
    coding => 
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
        .pipe {
          cs => csCodingValidator(cs)(coding)
        }


  implicit def coproductCodingValidator[H, T <: Coproduct](
    implicit
    cs: Coding.System[H],
    hVal: NegatableValidator[Issue.Builder,Coding[H]],
    tVal: NegatableValidator[Issue.Builder,Coding[T]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: T]] =
    coding =>
      (
        if (coding.system == cs.uri)
          hVal(coding.asInstanceOf[Coding[H]])
        else
          tVal(coding.asInstanceOf[Coding[T]])
      )
      .map(_.asInstanceOf[Coding[H :+: T]])
      

  
  implicit def terminalCoproductCodingValidator[H](
    implicit
    hVal: NegatableValidator[Issue.Builder,Coding[H]],
  ): NegatableValidator[Issue.Builder,Coding[H :+: CNil]] =
    coding =>
      hVal(coding.asInstanceOf[Coding[H]])
        .map(_.asInstanceOf[Coding[H :+: CNil]])


  implicit val proteinChangeValidator: Validator[Issue,Code[HGVS.Protein]] =
    code =>
      code.value must (matchRegex (HGVS.Protein.threeLetterCode) or contain ("?")) otherwise (
        Error(s"Ungültiger Code '${code}', erwarte 3-Buchstaben-Format für Amino-Säure") at "Amino-Säure-Austausch"
      ) map (_ => code)

      
  implicit def referenceValidator[T <: { def id: Id[_] }](
    implicit
    ts: Iterable[T],
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref => ref.resolve must be (defined) otherwise (
      Fatal(s"Nicht auflösbare Referenz-ID '${ref.id}' auf Objekt '${node.name}'")
    ) map (_ => ref)


  implicit def referenceValidatorTo[T <: { def id: Id[_] }](
    implicit
    t: T,
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref => ref.resolveOn(List(t)) must be (defined) otherwise (
      Fatal(s"Nicht auflösbare Referenz-ID '${ref.id}' auf Objekt '${node.name}'")
    ) map (_ => ref)


  implicit def geneAlterationReferenceValidator[T <: BaseVariant: Path.Node](
    implicit
    variants: Iterable[T],
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): NegatableValidator[Issue.Builder,GeneAlterationReference[T]] =
    ref => 
      (
        validate(ref.variant),
        validateOpt(ref.gene)
      )
      .errorsOr(ref)


  protected val ageRange = ClosedInterval(0 -> 130)

  implicit val patientValidator: Validator[Issue,Patient] =
    patient =>
      (
        patient.age.value.toInt must be (in (ageRange)) otherwise (
          Error(s"Patienten-Alter nicht im realistisch möglichen Intervall $ageRange Jahre") at "Alter"
        ),
        patient.healthInsurance.reference must be (defined) otherwise (MissingValue("Krankenkassen-IK")),
        option(patient.address.map(_.municipalityCode.value)) must matchRegex ("^\\d{5}$".r) otherwise (
          Error("Fehlerhaftes Format: erste 5 Ziffern erwartet") at "Amtlicher Gemeindeschlüssel"
        )
      )
      .errorsOr(patient) on patient


  protected def RecommendationValidator[T <: Recommendation: HasId: Path.Node](
    implicit
    patient: Patient,
    variants: Iterable[BaseVariant],
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,T] =
    rec =>
      (
        validate(rec.patient) at "Patient",
        rec.supportingVariants.getOrElse(List.empty) must be (nonEmpty) otherwise (
          MissingValue("Stützende molekulare Alteration(en)")
        ) andThen (
          validateEach(_) at "Stützende molekulare Alteration(en)"
        )
      )
      .errorsOr(rec) on rec


  protected def MedicationRecommendationValidator[Med, T <: MedicationRecommendation[Med]: HasId: Path.Node](
    implicit
    patient: Patient,
    variants: Iterable[BaseVariant],
    geneValidator: Validator[Issue.Builder,Coding[HGNC]],
    medicationValidator: Validator[Issue.Builder,Coding[Med]]
  ): Validator[Issue,T] =
    RecommendationValidator[T] combineWith { 
      rec => (validateEach(rec.medication.toList) at "Medikation").map(_ => rec)
    }


  implicit val studyRefValidator: Validator[Issue.Builder,ExternalReference[Study,Study.Registries]] = {
    import Study.Registries._

    val patterns: PartialFunction[URI,Regex] =
      Map(
        Coding.System[NCT].uri     -> "NCT\\d{8}".r,
        Coding.System[DRKS].uri    -> "DRKS000\\d{5}".r,
        Coding.System[EudraCT].uri -> "\\d{4}-\\d{6}-\\d{2}(-\\d{2})?".r,
        Coding.System[EUDAMED].uri -> ".+".r,
      )
      .orElse {
        case _ => ".+".r
      }

    ref => ref.id.value must matchRegex (patterns(ref.system)) otherwise (
      Error(s"Studien-ID ${ref.id.value} passt nicht zum erwarteten ID-Muster für ${ref.system}-Studien")
    ) map (_ => ref)
  }


  implicit def StudyRecommendationValidator[T <: StudyEnrollmentRecommendation: HasId: Path.Node](
    implicit
    patient: Patient,
    variants: Iterable[BaseVariant],
    geneValidator: Validator[Issue.Builder,Coding[HGNC]]
  ): Validator[Issue,T] =
    RecommendationValidator[T] combineWith {
      rec => (validateEach(rec.study) at "Studien-Referenz").map( _ => rec)
    }


  protected def TherapyValidator[T <: Therapy: HasId: Path.Node](
    implicit
    patient: Patient,
    diagnoses: Iterable[Diagnosis],
    recommendations: Iterable[TherapyRecommendation]
  ): Validator[Issue,T] =
    therapy =>
      (
        validate(therapy.patient) at "Patient",
        validateOpt(therapy.reason) at "Therapie-Grund (Diagnose)",
        validateOpt(therapy.basedOn) at "Therapie-Empfehlung",
      )
      .errorsOr(therapy) on therapy


  def RangeValidator[T](range: Interval[T]): Validator[Issue.Builder,T] =
    t => t must be (in (range)) otherwise (
      Error(s"Ungültiger Wert $t, nicht in Referenz-Bereich $range")
    ) 


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    valueValidator: Validator[Issue.Builder,O#ValueType]
  )(
    implicit patient: Patient
  ): Validator[Issue,O] =
    obs =>
      (
        validate(obs.patient) at "Patient",
        valueValidator(obs.value) at "Wert",
      )
      .errorsOr(obs) on obs


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    referenceRange: Interval[O#ValueType]
  )(
    implicit patient: Patient
  ): Validator[Issue,O] =
    ObservationValidator[O](RangeValidator(referenceRange))


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    implicit
    patient: Patient,
    valueValidator: Validator[Issue.Builder,O#ValueType]
  ): Validator[Issue,O] =
    ObservationValidator[O](valueValidator)


        
  def PatientRecordValidator[T <: PatientRecord]: Validator[Issue,T] = 
    record =>
      (
        validate(record.patient),
        (record.ngsReports.exists(_.nonEmpty) must be (true)) orElse (
          record.getCarePlans.exists(_.noSequencingPerformedReason.isDefined) must be (true) 
        ) otherwise (
          Error("Es sind keine(e) Sequenzierung-Bericht(e) vorhanden, aber auch kein Board-Beschluss mit Begründung, warum keine Sequenzierung beantragt worden ist")
        ) at "Sequenzier-Berichte/Board-Beschlüsse"
      )
      .errorsOr(record)



  private val hexString64 = "[a-fA-F0-9]{64}".r


  implicit def metadataValidator(
    implicit patient: Patient
  ): Validator[Issue,Submission.Metadata] =
    metadata =>
      (
        metadata.transferTAN.value must matchRegex (hexString64) otherwise (
          Error("Transfer-TAN entspricht nicht den Vorgaben: Hex-String mit 64 Zeichen") at "TAN (TVNk)"
        ),
        metadata.modelProjectConsent
          .provisions
          .find(_.purpose == Sequencing)
          .exists(_.`type` == Permit) must be (true) otherwise (Fatal("Fehlende Zustimmung zur Sequenzierung") at "MVH-Einwilligung"),
        ifDefined(metadata.modelProjectConsent.date)(
          d => 
            (
              all (metadata.modelProjectConsent.provisions.map(_.date)) must not (be (before (d))) otherwise (
                Error("Einwilligungsdatum kann nicht vor dem Vorlegen der Teilnahmeerklärung liegen") at "MVH-Einwilligung"
              )
            )
            .map(_ => d)
        ),
        (metadata.researchConsents.filter(_.nonEmpty) orElse metadata.reasonResearchConsentMissing) must be (defined) otherwise (
          MissingValue("Es muss entweder MII Forschungs-/Broad-Consent oder der Grund für dessen Fehlen vorhanden sein")
        ),
        ifDefined (metadata.researchConsents.filter(_.nonEmpty)){
          _ must (have (size (1))) otherwise (
            Error("Es kommen mehrere Consent-Ressourcen vor, aber es soll der Broad Consent des Index-Patient als 1 Consent-Ressource gebündelt übertragen werden") at "Broad Consent"
          ) andThen {
            consents =>
              val consent = consents.head
              (
                consent.status must be (BroadConsent.Status.Active) otherwise (
                  Warning(s"Unerwarteter Wert '${consent.status}', eigentlich '${BroadConsent.Status.Active}' erwartet") at "Consent.status"
                ), 
                consent.policyUri.replace("urn:oid:","") must be (in (BroadConsent.versions.keys)) otherwise (
                  Error(s"Ungültige OID '${consent.policyUri}', muss auf eine der zulässigen BC Versionen {${BroadConsent.versions.values.mkString(", ")}} verweisen.") at "Consent.policy.uri"
                ),
                option(consent.patient.map(_.id)) must be (patient.id) otherwise (
                  Warning("Patient-Referenz hat nicht dieselbe Patienten-ID wie am Patient-MDAT-Objekt") at "Consent.patient"
                )
              )
              .errorsOr(consents) on "Broad-Consent"
          }
        }
      )
      .errorsOr(metadata) on "Metadaten"



  def dataUploadValidator[T <: PatientRecord](
    implicit recordValidator: Validator[Issue,T]
  ): Validator[Issue,DataUpload[T]] = {

    case upload @ DataUpload(record,optMetadata) => 
      
      import de.dnpm.dip.service.mvh.extensions._

      implicit val patient = record.patient

      (
        ifDefined(optMetadata)(
          metadata =>
            (
              validate(metadata),
              record.mvhCarePlan must be (defined) otherwise (Error("Kein Board-Beschluss zum MVH-Einschluss vorhanden") at "Board-Beschlüsse") map (_.get) andThen {
                mvhCp => 
                  (
                    metadata.modelProjectConsent.provisions
                      .find(_.purpose == Sequencing)
                      .exists(provision => !(mvhCp.issuedOn isBefore provision.date)) must be (true) otherwise (
                        Error("MVH-Einschluss-Fallkonferenz darf nicht vor oder ohne Einwilligung zur Teilnahme stattgefunden haben") at "Datum der MVH-Einwilligung"
                      ),
                    (record.mvhSequencingReports must be (nonEmpty)) orElse (mvhCp.noSequencingPerformedReason must be (defined)) otherwise ( 
                      Error("Kein MVH-Sequenzierung-Bericht vorhanden, obwohl im Board-Beschluss nicht begründet ist, dass/warum keine MVH-Sequenzierung beantragt worden ist")
                        at "Sequenzier-Berichte"
                    )
                  )
                  .errorsOr(mvhCp)
              }, 
              metadata.`type` match {
                case Submission.Type.FollowUp =>
                  record.followUps.exists(_.nonEmpty) must be (true) otherwise (Error("Es ist 'Follow-up' als Meldungs-Typ deklariert, aber keine Follow-Up-Objekte vorhanden") at "Meldungs-Typ")

                case _ => true.validNel
              }
            )
            .errorsOr(metadata)
        ),
        validate(record)
      )
      .errorsOr(upload)

  }

}
