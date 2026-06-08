package openfeature.cats

import openfeature.model.*
import openfeature.bridge.ContextConverter
import cats.effect.IO
import munit.CatsEffectSuite

/** Round-trip verification of AttributeValue ↔ Java SDK Value conversion via the cats backend. */
class ValueRoundTripSpec extends CatsEffectSuite {

  private def roundTrip(attr: AttributeValue): AttributeValue = {
    val ctx     = EvaluationContext(None, Map("k" -> attr))
    val javaCtx = ContextConverter.toOpenFeature(ctx)
    val backCtx = ContextConverter.fromOpenFeature(javaCtx)
    backCtx.attributes("k")
  }

  test("BoolValue round-trips exactly") {
    assert(roundTrip(AttributeValue.BoolValue(true)) == AttributeValue.BoolValue(true))
    assert(roundTrip(AttributeValue.BoolValue(false)) == AttributeValue.BoolValue(false))
  }

  test("StringValue round-trips for non-empty strings") {
    val s = "hello-world"
    assert(roundTrip(AttributeValue.StringValue(s)) == AttributeValue.StringValue(s))
  }

  test("IntValue round-trips exactly") {
    assert(roundTrip(AttributeValue.IntValue(42)) == AttributeValue.IntValue(42))
    assert(roundTrip(AttributeValue.IntValue(-1)) == AttributeValue.IntValue(-1))
    assert(roundTrip(AttributeValue.IntValue(0)) == AttributeValue.IntValue(0))
  }

  test("LongValue (above Int range) round-trips as LongValue") {
    val l = Int.MaxValue.toLong + 1L
    roundTrip(AttributeValue.LongValue(l)) match {
      case AttributeValue.LongValue(v) => assert(v == l)
      case other                       => fail(s"expected LongValue, got $other")
    }
  }

  test("DoubleValue (finite, fractional) round-trips exactly") {
    val d = 3.14159
    roundTrip(AttributeValue.DoubleValue(d)) match {
      case AttributeValue.DoubleValue(v) => assert(v == d)
      case other                         => fail(s"expected DoubleValue, got $other")
    }
  }

  test("LongValue(Long.MaxValue) lands as DoubleValue (saturation boundary)") {
    assert(roundTrip(AttributeValue.LongValue(Long.MaxValue)).isInstanceOf[AttributeValue.DoubleValue])
  }

  test("nested struct preserves leaf values via typed accessors") {
    val struct = AttributeValue.StructValue(
      Map(
        "bool"   -> AttributeValue.BoolValue(true),
        "string" -> AttributeValue.StringValue("cats"),
        "int"    -> AttributeValue.IntValue(7),
        "double" -> AttributeValue.DoubleValue(2.718)
      )
    )
    roundTrip(struct) match {
      case AttributeValue.StructValue(fields) =>
        assert(fields("bool").asBoolean.contains(true))
        assert(fields("string").asString.contains("cats"))
        assert(fields("int").asInt.contains(7))
        assert(fields("double").asDouble.nonEmpty)
      case other => fail(s"expected StructValue, got $other")
    }
  }

  test("ListValue of ints round-trips") {
    val list = AttributeValue.ListValue(List(AttributeValue.IntValue(1), AttributeValue.IntValue(2)))
    roundTrip(list) match {
      case AttributeValue.ListValue(items) =>
        assert(items.flatMap(_.asInt) == List(1, 2))
      case other => fail(s"expected ListValue, got $other")
    }
  }
}
