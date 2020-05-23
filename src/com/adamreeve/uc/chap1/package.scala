package com.adamreeve.uc

import scala.collection.immutable.HashMap
import scala.collection.mutable

package object chap1 {
  type Environment = Map[String, ValueExpression]
  val emptyEnvironment: Environment = new HashMap[String, ValueExpression]()
}
