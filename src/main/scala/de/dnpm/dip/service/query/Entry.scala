package de.dnpm.dip.service.query



import play.api.libs.json.{
  Json,
  JsPath,
  Reads,
  OWrites,
  Writes,
  Format,
  OFormat
}


/*
 * Custom helper type to represent Map entries, i.e. pairs,
 * in order to have a more explicit JSON serialization format:
 *
 * Default JSON representation of Map[K,V] is a JsArray of sub-JsArrays,
 * in which the first element is the key and second entry the value
 *
 * With this Entry type the key and value are fields on a JsObject
 * { 
 *   "key": <K>,
 *   "value": <V>
 * }
*/
//final case class Entry[K,V]
final case class Entry[+K,+V]
(
  key: K,
  value: V,
  children: Option[Seq[Entry[K,V]]] = None  // to allow building hierarchical structures
)


object Entry 
{

  import play.api.libs.functional.syntax._


  // Explicit Format definition required because of recursive type
  implicit def writes[K: Writes, V: Writes]: OWrites[Entry[K,V]] =
    (
      (JsPath \ "key").write[K] and
      (JsPath \ "value").write[V] and
      (JsPath \ "children").lazyWriteNullable(Writes.of[Seq[Entry[K,V]]]) // lazyWrite to handle recursivity
    )(
      unlift(Entry.unapply[K,V](_))
    ) 

  implicit def reads[K: Reads, V: Reads]: Reads[Entry[K,V]] =
    (
      (JsPath \ "key").read[K] and
      (JsPath \ "value").read[V] and
      (JsPath \ "children").lazyReadNullable(Reads.of[Seq[Entry[K,V]]]) // lazyRead to handle recursivity
    )(
      Entry(_,_,_)
    ) 

/*
  implicit def format[K: Format, V: Format]: OFormat[Entry[K,V]] =
    (
      (JsPath \ "key").format[K] and
      (JsPath \ "value").format[V] and
      (JsPath \ "children").lazyFormatNullable(Format.of[Seq[Entry[K,V]]]) // lazyFormat to handle recursivity
    )(
      Entry.apply _,
      unlift(Entry.unapply)
    ) 
*/


  // Order Entries by decreasing number of occurrence
  implicit def entryIntOrdering[K]: Ordering[Entry[K,Int]] =
    Ordering[Int]
      .on[Entry[K,Int]](_.value)
      .reverse


//  implicit def writes[K: Writes, V: Writes]: OWrites[Entry[K,V]] =
//    Json.writes[Entry[K,V]]
}

