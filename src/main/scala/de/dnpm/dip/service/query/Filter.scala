package de.dnpm.dip.service.query


import de.dnpm.dip.model.Patient


trait Filters[-PatientRecord <: { def patient: Patient }]
extends (PatientRecord => Boolean)
{
  def patientFilter: PatientFilter
}

