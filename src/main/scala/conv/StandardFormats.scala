package com.alexknvl.libconfig.conv

import com.alexknvl.libconfig.ast._

/**
  * Provides the ConfigFormats for the non-collection standard types.
 */
trait StandardFormats {
  private[conv] type CF[T] = ConfigFormat[T] // simple alias for reduced verbosity

  implicit def tuple1Format[A :CF] = new ConfigFormat[Tuple1[A]] {
    def write(t: Tuple1[A]) = ConfigList(List(t._1.toConfigValue))
    def read(value: ConfigValue): Tuple1[A] = value match {
      case ConfigList(Seq(a)) => Tuple1(a.to[A])
      case x => deserializationError("Expected Tuple1 as ConfigList, but got " + x)
    }
  }

  implicit def tuple2Format[A :CF, B :CF] = new ConfigFormat[(A, B)] {
    def write(t: (A, B)) = ConfigList(List(t._1.toConfigValue, t._2.toConfigValue))
    def read(value: ConfigValue): (A, B) = value match {
      case ConfigList(Seq(a, b)) => (a.to[A], b.to[B])
      case x => deserializationError("Expected Tuple2 as ConfigList, but got " + x)
    }
  }

  implicit def listFormat[T :CF] = new ConfigFormat[List[T]] {
    def write(list: List[T]) = ConfigArray(list.map(_.toConfigValue))
    def read(value: ConfigValue): List[T] = value match {
      case ConfigArray(items) => items.map(_.to[T]).toList
      case x => deserializationError("Expected List as ConfigArray, but got " + x)
    }
  }

  implicit def arrayFormat[T :CF :scala.reflect.ClassTag] = new ConfigFormat[Array[T]] {
    def write(array: Array[T]) = ConfigArray(array.map(_.toConfigValue).toList)
    def read(value: ConfigValue): Array[T] = value match {
      case ConfigArray(items) => items.map(_.to[T]).toArray[T]
      case x => deserializationError("Expected Array as ConfigArray, but got " + x)
    }
  }

  implicit def mapFormat[V :CF] = new ConfigFormat[Map[String, V]] {
    def write(m: Map[String, V]) = ConfigGroup(m map { case(k, v) => (k, v.toConfigValue) })
    def read(value: ConfigValue): Map[String, V] = value match {
      case ConfigGroup(items) => items.map { case(k, v) => (k, v.to[V]) }
      case x => deserializationError("Expected Map as ConfigGroup, but got " + x)
    }
  }

  import collection.{immutable => imm}

  implicit def immIterableFormat[T :ConfigFormat]   = viaList[imm.Iterable[T], T](list => imm.Iterable(list :_*))
  implicit def immSeqFormat[T :ConfigFormat]        = viaList[imm.Seq[T], T](list => imm.Seq(list :_*))
  implicit def immIndexedSeqFormat[T :ConfigFormat] = viaList[imm.IndexedSeq[T], T](list => imm.IndexedSeq(list :_*))
  implicit def immLinearSeqFormat[T :ConfigFormat]  = viaList[imm.LinearSeq[T], T](list => imm.LinearSeq(list :_*))
  implicit def immSetFormat[T :ConfigFormat]        = viaList[imm.Set[T], T](list => imm.Set(list :_*))
  implicit def vectorFormat[T :ConfigFormat]        = viaList[Vector[T], T](list => Vector(list :_*))

  import collection._

  implicit def iterableFormat[T :ConfigFormat]   = viaList[Iterable[T], T](list => Iterable(list :_*))
  implicit def seqFormat[T :ConfigFormat]        = viaList[Seq[T], T](list => Seq(list :_*))
  implicit def indexedSeqFormat[T :ConfigFormat] = viaList[IndexedSeq[T], T](list => IndexedSeq(list :_*))
  implicit def linearSeqFormat[T :ConfigFormat]  = viaList[LinearSeq[T], T](list => LinearSeq(list :_*))
  implicit def setFormat[T :ConfigFormat]        = viaList[Set[T], T](list => Set(list :_*))

  /**
    * A ConfigFormat construction helper that creates a ConfigFormat for an Iterable type I from a builder function
    * List => I.
   */
  def viaList[I <: Iterable[T], T :CF](f: List[T] => I): ConfigFormat[I] = new ConfigFormat[I] {
    def write(iterable: I) = ConfigArray(iterable.map(_.toConfigValue).toSeq)
    def read(value: ConfigValue) = value match {
      case ConfigArray(items) => f(items.map(_.to[T]).toList)
      case x => deserializationError("Expected Collection as ConfigArray, but got " + x)
    }
  }
}
