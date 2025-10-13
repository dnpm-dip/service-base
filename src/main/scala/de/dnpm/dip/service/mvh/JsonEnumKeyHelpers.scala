package de.dnpm.dip.service.mvh


import scala.util.Try
import play.api.libs.json.{
  JsResult,
  JsError,
  KeyReads,
  KeyWrites,
}


/*
 * Custom utility to ensure representation of Map with Enum.Value keys and arbitrary values as JsObject 
 * {
 *   "enum1": ...,
 *   "enum2": ...,
 *   "enum3": ...,
 * }
 * because the default derived Format[Map...] seems buggy w.r.t to write/read:
 * The Map is written as an Array of pairs [[key1,value1],[key2,value2],...]
 * but the Reads expects a JsObject
 */
trait JsonEnumKeyHelpers
{

  implicit def enumKeyReads[E <: Enumeration](
    implicit witness: shapeless.Witness.Aux[E]
  ): KeyReads[E#Value] =
    KeyReads(k =>
      JsResult.fromTry(
        Try(witness.value.withName(k)),
        _ => JsError(s"Invalid enum value, expected one of {${witness.value.values.mkString(",")}}")
      )
    ) 

  implicit def enumKeyWrites[E <: Enumeration]: KeyWrites[E#Value] =
    KeyWrites(_.toString)

}
