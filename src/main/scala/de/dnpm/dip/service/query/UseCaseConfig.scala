package de.dnpm.dip.service.query


import de.dnpm.dip.model.{PatientRecord => PatRec}


trait UseCaseConfig 
{

  type PatientRecord <: PatRec

  type Criteria

//  type Filter <: Filters[PatientRecord]

//  type Results <: ResultSet[PatientRecord,Criteria,Filter]

  type Results <: ResultSet[PatientRecord,Criteria]

  type Filter = Results#Filter

}
