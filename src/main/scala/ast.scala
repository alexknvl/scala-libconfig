package com.alexknvl.libconfig.ast

import com.alexknvl.libconfig.parser.PathParser

sealed abstract class PathItem
case class NamedItem(name: String) extends PathItem
case class IndexedItem(index: Int) extends PathItem

sealed abstract class ConfigValue {
  def apply(index: Int): Option[ConfigValue] = None
  def apply(path: String): Option[ConfigValue] =
    PathParser(path) match {
      case Some(p) =>
        println(p)
        this.apply(p)
      case None => None
    }
  def apply(path: Seq[PathItem]): Option[ConfigValue] =
    if (path.size == 0) Some(this)
    else None
}

case class ConfigInt(value: Int) extends ConfigValue
case class ConfigLong(value: Long) extends ConfigValue
case class ConfigFloat(value: Double) extends ConfigValue
case class ConfigBoolean(value: Boolean) extends ConfigValue
case class ConfigString(value: String) extends ConfigValue

case class ConfigArray(items: Seq[ConfigValue]) extends ConfigValue {
  override def apply(index: Int) =
    if (index < items.length) Some(items(index))
    else None
  override def apply(path: Seq[PathItem]): Option[ConfigValue] =
    if (path.size == 0) Some(this)
    else {
      path.head match {
        case NamedItem(_) => None
        case IndexedItem(index) if index < items.length => items(index).apply(path.tail)
      }
    }
}
case class ConfigList(items: Seq[ConfigValue]) extends ConfigValue {
  override def apply(index: Int) =
    if (index < items.length) Some(items(index))
    else None
  override def apply(path: Seq[PathItem]): Option[ConfigValue] =
    if (path.size == 0) Some(this)
    else {
      path.head match {
        case NamedItem(_) => None
        case IndexedItem(index) if index < items.length => items(index).apply(path.tail)
      }
    }
}
case class ConfigGroup(items: Map[String, ConfigValue]) extends ConfigValue {
  override def apply(path: Seq[PathItem]): Option[ConfigValue] = {
    if (path.size == 0) Some(this)
    else {
      path.head match {
        case NamedItem(name) if items contains name =>
          items(name).apply(path.tail)
        case IndexedItem(_) => None
      }
    }
  }
}

object ConfigInt {
  def apply(value: String) = {
    if (value.startsWith("0x")) new ConfigInt(java.lang.Integer.parseInt(value.drop(2), 16))
    else new ConfigInt(value.toInt)
  }
}
object ConfigLong {
  def apply(value: String) = {
    if (value.startsWith("0x")) new ConfigLong(java.lang.Long.parseLong(value.drop(2), 16))
    else new ConfigLong(value.toLong)
  }
}
object ConfigFloat {
  def apply(value: String) = new ConfigFloat(value.toDouble)
}
