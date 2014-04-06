package com.alexknvl.libconfig.conv

import com.alexknvl.libconfig.ast._

trait BasicFormats {
  implicit object IntConfigFormat extends ConfigFormat[Int] {
    def write(x: Int) = ConfigInt(x)
    def read(value: ConfigValue) = value match {
      case ConfigInt(x) => x.intValue
      case x => deserializationError("Expected Int as ConfigInt, but got " + x)
    }
  }

  implicit object LongConfigFormat extends ConfigFormat[Long] {
    def write(x: Long) = ConfigLong(x)
    def read(value: ConfigValue) = value match {
      case ConfigLong(x) => x.longValue
      case x => deserializationError("Expected Long as ConfigLong, but got " + x)
    }
  }

  implicit object FloatConfigFormat extends ConfigFormat[Float] {
    def write(x: Float) = ConfigFloat(x)
    def read(value: ConfigValue) = value match {
      case ConfigFloat(x) => x.floatValue
      case x => deserializationError("Expected Float as ConfigFloat, but got " + x)
    }
  }

  implicit object DoubleConfigFormat extends ConfigFormat[Double] {
    def write(x: Double) = ConfigFloat(x)
    def read(value: ConfigValue) = value match {
      case ConfigFloat(x) => x.floatValue
      case x => deserializationError("Expected Double as ConfigFloat, but got " + x)
    }
  }

  implicit object ByteConfigFormat extends ConfigFormat[Byte] {
    def write(x: Byte) = ConfigInt(x)
    def read(value: ConfigValue) = value match {
      case ConfigInt(x) => x.byteValue
      case x => deserializationError("Expected Byte as ConfigInt, but got " + x)
    }
  }

  implicit object ShortConfigFormat extends ConfigFormat[Short] {
    def write(x: Short) = ConfigInt(x)
    def read(value: ConfigValue) = value match {
      case ConfigInt(x) => x.shortValue
      case x => deserializationError("Expected Short as ConfigInt, but got " + x)
    }
  }

  implicit object UnitConfigFormat extends ConfigFormat[Unit] {
    def write(x: Unit) = ConfigList(Nil)
    def read(value: ConfigValue) = value match {
      case ConfigList(Nil) => ()
      case x => deserializationError("Expected Unit as empty ConfigList, but got " + x)
    }
  }

  implicit object BooleanConfigFormat extends ConfigFormat[Boolean] {
    def write(x: Boolean) = ConfigBoolean(x)
    def read(value: ConfigValue): Boolean = value match {
      case ConfigBoolean(x) => x
      case x => deserializationError("Expected ConfigBoolean, but got " + x)
    }
  }

  implicit object CharConfigFormat extends ConfigFormat[Char] {
    def write(x: Char) = ConfigString(String.valueOf(x))
    def read(value: ConfigValue) = value match {
      case ConfigString(x) if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character ConfigString, but got " + x)
    }
  }

  implicit object StringConfigFormat extends ConfigFormat[String] {
    def write(x: String) = {
      require(x ne null)
      ConfigString(x)
    }
    def read(value: ConfigValue) = value match {
      case ConfigString(x) => x
      case x => deserializationError("Expected String as ConfigString, but got " + x)
    }
  }

  implicit object SymbolConfigFormat extends ConfigFormat[Symbol] {
    def write(x: Symbol) = ConfigString(x.name)
    def read(value: ConfigValue) = value match {
      case ConfigString(x) => Symbol(x)
      case x => deserializationError("Expected Symbol as ConfigString, but got " + x)
    }
  }
}
