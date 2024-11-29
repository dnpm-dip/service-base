package de.dnpm.dip.service.query



import play.api.libs.json.{
  JsPath,
  Reads,
  OWrites,
  Writes
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


  // Order Entries by decreasing number of occurrence
  implicit def entryOrdering[K,V: Ordering]: Ordering[Entry[K,V]] =
    Ordering[V]
      .on[Entry[K,V]](_.value)

}
