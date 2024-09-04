package de.dnpm.dip.service.query


import de.dnpm.dip.model.Patient


trait Filters[+PatientRecord <: { def patient: Patient }]
{
  def patient: PatientFilter
}

