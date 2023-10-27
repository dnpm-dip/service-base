package de.dnpm.dip.service.query


import de.dnpm.dip.model.Patient


trait UseCaseConfig 
{

  type PatientRecord <: { def patient: Patient }

  type Criteria

  type Filters <: Query.Filters

  type Results <: ResultSet[PatientRecord,Criteria]

}
