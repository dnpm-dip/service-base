package de.dnpm.dip.service.mvh


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import json.Schema
import org.scalatest.matchers.must.Matchers._
import json.schema.Version._
import com.github.andyglow.jsonschema.AsPlay._
import play.api.libs.json.Json.prettyPrint
import de.dnpm.dip.service.DataUpload.Schemas


class JsonSchemaTests
extends AnyFlatSpec
with Inspectors
with Schemas 
{

  "JSON Schema derivation for Submission.Type" must "have worked" in {

    val schema =
      Schema[Submission.Type.Value].asPlay(Draft12("SubmissionType"))
        .pipe(prettyPrint(_))

    val capitalizedSubmissionTypes =
      Submission.Type
        .values.map(_.toString)
        .map(v => v.substring(0,1).toUpperCase + v.substring(1))

    forAll(capitalizedSubmissionTypes){t => schema must not include (t)}

  }

}
