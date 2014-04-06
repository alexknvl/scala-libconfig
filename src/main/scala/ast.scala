package com.alexknvl.libconfig.ast

import scala.util.control.NonFatal

sealed abstract class PathItem
case class NamedItem(name: String) extends PathItem
case class IndexedItem(index: Int) extends PathItem

object PathParser {
  def apply(path: String): Option[Seq[PathItem]] = {
    try {
      val items = path.split("\\.")

      Some(items.map { item: String =>
        if (item.head == '[' && item.last == ']') {
          val indexStr = item.slice(1, item.length - 1)
          IndexedItem(Integer.parseInt(indexStr))
        } else {
          NamedItem(item)
        }
      })
    } catch {
      case NonFatal(e) => None
    }
  }
}

object Getters {
  private[this] def getItem(cv: ConfigValue, item: PathItem): Option[ConfigValue] = (cv, item) match {
    case (ConfigGroup(items), NamedItem(name)) => items.get(name)
    case (ConfigArray(items), IndexedItem(index)) if index < items.length => Some(items(index))
    case (ConfigList(items), IndexedItem(index)) if index < items.length => Some(items(index))
    case _ => None
  }
  private[this] def getPath(cv: ConfigValue, path: Seq[PathItem]): Option[ConfigValue] = path match {
    case Seq() => Some(cv)
    case item +: rest => getItem(cv, item).flatMap {getPath(_, rest)}
  }

  implicit class ConfigValueWithGetters(base: ConfigValue) {
    def get(path: String): Option[ConfigValue] = PathParser(path).flatMap(getPath(base, _))
    def getOrElse(path: String, defVal: ConfigValue): ConfigValue = get(path).getOrElse(defVal)
    def apply(path: String): ConfigValue = get(path).get
  }
}

sealed abstract class ConfigValue

case class ConfigInt(value: Int) extends ConfigValue
case class ConfigLong(value: Long) extends ConfigValue
case class ConfigFloat(value: Double) extends ConfigValue
case class ConfigBoolean(value: Boolean) extends ConfigValue
case class ConfigString(value: String) extends ConfigValue

case class ConfigArray(items: Seq[ConfigValue]) extends ConfigValue
case class ConfigList(items: Seq[ConfigValue]) extends ConfigValue
case class ConfigGroup(items: Map[String, ConfigValue]) extends ConfigValue

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
