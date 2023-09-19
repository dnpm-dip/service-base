package de.dnpm.dip.service.query



trait UseCaseConfig 
{

  type PatientRecord

  type Criteria

  type Filters <: Query.Filters

  type Results <: ResultSet[PatientRecord,Criteria]

}
