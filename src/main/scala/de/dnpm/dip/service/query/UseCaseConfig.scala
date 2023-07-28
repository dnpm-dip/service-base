package de.dnpm.dip.service.query



trait UseCaseConfig 
{

  type PatientRecord

  type Parameters

  type Filters <: Query.Filters

  type Results <: ResultSet

}
