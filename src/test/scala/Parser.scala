package com.alexknvl.libconfig.parser

import com.alexknvl.libconfig.ast._
import org.parboiled2.Parser.DeliveryScheme.Throw

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

class ParserTest extends FunSuite with Matchers {
  test("Parsing simple types.") {
    new ConfigParser("true").Value.run() should be (ConfigBoolean(true))
    new ConfigParser("false").Value.run() should be (ConfigBoolean(false))
    new ConfigParser("123").Value.run() should be (ConfigInt(123))
    new ConfigParser("123L").Value.run() should be (ConfigLong(123))
    new ConfigParser("123.01234").Value.run() should be (ConfigFloat(123.01234))
    new ConfigParser("1e3").Value.run() should be (ConfigFloat(1e3))
    new ConfigParser("1.2e3").Value.run() should be (ConfigFloat(1.2e3))
    new ConfigParser("0x22").Value.run() should be (ConfigInt(34))
    new ConfigParser("\"abc\"").Value.run() should be (ConfigString("abc"))
  }

  test("Parsing settings.") {
    new ConfigParser("name: 123").Setting.run() should be ("name" -> ConfigInt(123))
    new ConfigParser("name = 123").Setting.run() should be ("name" -> ConfigInt(123))
  }

  test("Parsing arrays and lists.") {
    new ConfigParser("(12, 123L, true)").Value.run() should be
      (ConfigList(Seq(ConfigInt(12), ConfigLong(123), ConfigBoolean(true))))
    new ConfigParser("[12, 123L, true]").Value.run() should be
      (ConfigArray(Seq(ConfigInt(12), ConfigLong(123), ConfigBoolean(true))))
  }

  test("Parsing groups.") {
    new ConfigParser("{ name: 123L }").Value.run() should be
      (ConfigGroup(Map("name" -> ConfigLong(123))))
    new ConfigParser("{ name: 123L; }").Value.run() should be
      (ConfigGroup(Map("name" -> ConfigLong(123))))
    new ConfigParser("{ name: 123L name2 = 1 }").Value.run() should be
      (ConfigGroup(Map("name" -> ConfigLong(123), "name2" -> ConfigInt(1))))
    new ConfigParser("{ name: 123L; name2 = 1 }").Value.run() should be
      (ConfigGroup(Map("name" -> ConfigLong(123), "name2" -> ConfigInt(1))))
  }
}
