package com.alexknvl.libconfig

import com.alexknvl.libconfig.ast._

import scala.annotation.implicitNotFound

package object conv {
  def configReader[T](implicit reader: ConfigReader[T]) = reader
  def configWriter[T](implicit writer: ConfigWriter[T]) = writer

  def deserializationError(msg: String, cause: Throwable = null) = throw new DeserializationException(msg, cause)
  def serializationError(msg: String) = throw new SerializationException(msg)

  implicit class EnrichedConfigValue(value: ConfigValue) {
    def to[T: ConfigReader]: T = configReader[T].read(value)
  }

  implicit class PimpedAny[T](any: T) {
    def toConfigValue(implicit writer: ConfigWriter[T]): ConfigValue = writer.write(any)
  }
}

package conv {
  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

  /**
   * Provides the Config deserialization for type T.
   */
  @implicitNotFound(msg = "Cannot find ConfigReader or ConfigFormat type class for ${T}")
  trait ConfigReader[T] {
    def read(Config: ConfigValue): T
  }

  /**
    * Provides the Config serialization for type T.
   */
  @implicitNotFound(msg = "Cannot find ConfigWriter or ConfigFormat type class for ${T}")
  trait ConfigWriter[T] {
    def write(obj: T): ConfigValue
  }

  /**
   * Provides the Config deserialization and serialization for type T.
   */
  trait ConfigFormat[T] extends ConfigReader[T] with ConfigWriter[T]
}
