package com.alexknvl.libconfig

import com.alexknvl.libconfig.ast._

import annotation.implicitNotFound

package object conv {
  def configReader[T](implicit reader: ConfigReader[T]) = reader
  def configWriter[T](implicit writer: ConfigWriter[T]) = writer

  def deserializationError(msg: String, cause: Throwable = null) = throw new DeserializationException(msg, cause)
  def serializationError(msg: String) = throw new SerializationException(msg)

  implicit class EnrichedConfigValue(value: ConfigValue) {
    def to[T: ConfigReader]: T = configReader[T].read(value)
  }

  implicit class PimpedAny[T](any: T) {
    def toJson(implicit writer: ConfigWriter[T]): ConfigValue = writer.write(any)
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

  // object ConfigReader {
  //   implicit def func2Reader[T](f: ConfigValue => T): ConfigReader[T] = new ConfigReader[T] {
  //     def read(Config: ConfigValue) = f(Config)
  //   }
  // }

  /**
    * Provides the Config serialization for type T.
   */
  @implicitNotFound(msg = "Cannot find ConfigWriter or ConfigFormat type class for ${T}")
  trait ConfigWriter[T] {
    def write(obj: T): ConfigValue
  }

  // object ConfigWriter {
  //   implicit def func2Writer[T](f: T => ConfigValue): ConfigWriter[T] = new ConfigWriter[T] {
  //     def write(obj: T) = f(obj)
  //   }
  // }

  /**
   * Provides the Config deserialization and serialization for type T.
   */
  trait ConfigFormat[T] extends ConfigReader[T] with ConfigWriter[T]

  /**
   * A special ConfigReader capable of reading a legal Config root object, i.e. either a Config array or a Config object.
   */
  @implicitNotFound(msg = "Cannot find RootConfigReader or RootConfigFormat type class for ${T}")
  trait RootConfigReader[T] extends ConfigReader[T]

  /**
   * A special ConfigWriter capable of writing a legal Config root object, i.e. either a Config array or a Config object.
   */
  @implicitNotFound(msg = "Cannot find RootConfigWriter or RootConfigFormat type class for ${T}")
  trait RootConfigWriter[T] extends ConfigWriter[T]

  /**
   * A special ConfigFormat signaling that the format produces a legal Config root object, i.e. either a Config array
   * or a Config object.
   */
  trait RootConfigFormat[T] extends ConfigFormat[T] with RootConfigReader[T] with RootConfigWriter[T]
}
