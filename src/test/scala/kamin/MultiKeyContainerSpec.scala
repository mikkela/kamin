package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MultiKeyContainerSpec extends AnyFunSpec with Matchers {

  describe("A MultiKeyContainer") {
    it("should allow inserting and retrieving single-component keys") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1")("Value1")
      container.get("Key1") shouldEqual Some("Value1")
    }

    it("should return None when looking up a non-existing single-component key") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1")("Value1")
      container.get("NonExistentKey") shouldEqual None
    }

    it("should allow inserting and retrieving multi-component keys") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1", "SubKey1")("Value1-SubKey1")
      container.put("Key1", "SubKey2")("Value1-SubKey2")

      container.get("Key1", "SubKey1") shouldEqual Some("Value1-SubKey1")
      container.get("Key1", "SubKey2") shouldEqual Some("Value1-SubKey2")
    }

    it("should return None for non-existing multi-component keys") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1", "SubKey1")("Value1-SubKey1")

      container.get("Key1", "NonExistentSubKey") shouldEqual None
      container.get("NonExistentKey", "SubKey1") shouldEqual None
    }

    it("should retrieve the first value when the key exists both as a single-component and multi-component key") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1")("SingleValue")
      container.put("Key1", "SubKey1")("MultiValue")

      container.get("Key1") shouldEqual Some("SingleValue")
    }

    it("should retrieve all values associated with a given first-level key") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1")("SingleValue")
      container.put("Key1", "SubKey1")("MultiValue1")
      container.put("Key1", "SubKey2")("MultiValue2")

      val values = container.getAllWithPrefix("Key1")

      values should contain allOf("SingleValue", "MultiValue1", "MultiValue2")
    }

    it("should retrieve all values associated with a multi-component prefix") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1", "SubKey1", "SubSubKey1")("Value1")
      container.put("Key1", "SubKey1", "SubSubKey2")("Value2")
      container.put("Key1", "SubKey2")("Value3")

      val values = container.getAllWithPrefix("Key1", "SubKey1")

      values should contain allOf("Value1", "Value2")
      values should not contain ("Value3")
    }

    it("should return an empty sequence for a non-existent prefix") {
      val container = MultiKeyContainer[String, String]()

      container.put("Key1")("SingleValue")
      container.put("Key1", "SubKey1")("MultiValue1")

      val values = container.getAllWithPrefix("NonExistentKey")
      values shouldBe empty
    }
  }
}
