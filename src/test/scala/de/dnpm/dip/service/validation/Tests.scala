package de.dnpm.dip.service.validation


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import Issue.Path


class Tests extends AnyFlatSpec
{

  "Path" must "not contain double slashes '//'" in {

     val path =
       (Path.root / "path" / "to" / "node")
         .pipe(_.toString)
         .pipe(Path.from)
         .pipe(_.toString)

     path must not include ("//") 
  }



}
