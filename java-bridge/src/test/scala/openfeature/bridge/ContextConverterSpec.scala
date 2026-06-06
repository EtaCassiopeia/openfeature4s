package openfeature.bridge

import openfeature.model.{AttributeValue, EvaluationContext}
import zio.test._

/** Round-trip coverage of AttributeValue ↔ OpenFeature Java SDK Value conversion.
  *
  * The Java SDK normalises all numerics to Double, so IntValue and LongValue are mediated through Double and may come
  * back as a different concrete case. The properties below pin the contract callers rely on — that values round-trip as
  * observed through the typed accessors (asBoolean, asString, asLong, asDouble, etc.).
  */
object ContextConverterSpec extends ZIOSpecDefault {

  private def roundTrip(attr: AttributeValue): AttributeValue = {
    val ctx     = EvaluationContext(None, Map("k" -> attr))
    val javaCtx = ContextConverter.toOpenFeature(ctx)
    val backCtx = ContextConverter.fromOpenFeature(javaCtx)
    backCtx.attributes("k")
  }

  private val finiteFractionalDouble: Gen[Any, Double] =
    Gen.double(-1e10, 1e10).filter(d => !d.isWhole && !d.isNaN && !d.isInfinity)

  private val largeLong: Gen[Any, Long] =
    Gen.long(Int.MaxValue.toLong + 1L, (1L << 53) - 1L)

  private val nonEmptyString: Gen[Any, String] =
    Gen.string1(Gen.unicodeChar)

  def spec = suite("ContextConverter")(
    test("BoolValue round-trips exactly") {
      check(Gen.boolean) { b =>
        roundTrip(AttributeValue.BoolValue(b)) match {
          case AttributeValue.BoolValue(v) => assertTrue(v == b)
          case other                       => assertTrue(false) ?? s"expected BoolValue, got $other"
        }
      }
    },
    test("StringValue round-trips for non-empty unicode strings") {
      check(nonEmptyString) { s =>
        roundTrip(AttributeValue.StringValue(s)) match {
          case AttributeValue.StringValue(v) => assertTrue(v == s)
          case other                         => assertTrue(false) ?? s"expected StringValue, got $other"
        }
      }
    },
    test("IntValue round-trips exactly") {
      check(Gen.int) { i =>
        roundTrip(AttributeValue.IntValue(i)) match {
          case AttributeValue.IntValue(v) => assertTrue(v == i)
          case other                      => assertTrue(false) ?? s"expected IntValue, got $other"
        }
      }
    },
    test("LongValue (large enough to be distinct from Int) round-trips as LongValue") {
      check(largeLong) { l =>
        roundTrip(AttributeValue.LongValue(l)) match {
          case AttributeValue.LongValue(v) => assertTrue(v == l)
          case other                       => assertTrue(false) ?? s"expected LongValue($l), got $other"
        }
      }
    },
    test("DoubleValue (finite, fractional) round-trips exactly") {
      check(finiteFractionalDouble) { d =>
        roundTrip(AttributeValue.DoubleValue(d)) match {
          case AttributeValue.DoubleValue(v) => assertTrue(v == d)
          case other                         => assertTrue(false) ?? s"expected DoubleValue, got $other"
        }
      }
    },
    test("DoubleValue NaN survives as NaN") {
      val r = roundTrip(AttributeValue.DoubleValue(Double.NaN))
      assertTrue(r match {
        case AttributeValue.DoubleValue(v) => v.isNaN
        case _                             => false
      })
    },
    test("LongValue(Long.MaxValue) lands as DoubleValue (documents saturation behaviour)") {
      val r = roundTrip(AttributeValue.LongValue(Long.MaxValue))
      assertTrue(r.isInstanceOf[AttributeValue.DoubleValue])
    },
    test("nested struct preserves leaf values via typed accessors") {
      val structGen: Gen[Any, AttributeValue] = for {
        b <- Gen.boolean
        s <- nonEmptyString
        i <- Gen.int
        d <- finiteFractionalDouble
      } yield AttributeValue.StructValue(
        Map(
          "bool"   -> AttributeValue.BoolValue(b),
          "string" -> AttributeValue.StringValue(s),
          "int"    -> AttributeValue.IntValue(i),
          "double" -> AttributeValue.DoubleValue(d)
        )
      )
      check(structGen) { struct =>
        val orig = struct.asStruct.getOrElse(Map.empty)
        roundTrip(struct) match {
          case AttributeValue.StructValue(fields) =>
            assertTrue(
              fields("bool").asBoolean == orig("bool").asBoolean,
              fields("string").asString == orig("string").asString,
              fields("int").asInt == orig("int").asInt,
              fields("double").asDouble == orig("double").asDouble
            )
          case other => assertTrue(false) ?? s"expected StructValue, got $other"
        }
      }
    },
    test("ListValue of ints round-trips") {
      val listGen: Gen[Any, AttributeValue] =
        Gen.listOfBounded(0, 8)(Gen.int.map(AttributeValue.IntValue.apply)).map(AttributeValue.ListValue.apply)
      check(listGen) { list =>
        val orig = list.asList.getOrElse(Nil)
        roundTrip(list) match {
          case AttributeValue.ListValue(items) =>
            assertTrue(items.flatMap(_.asInt) == orig.flatMap(_.asInt))
          case other => assertTrue(false) ?? s"expected ListValue, got $other"
        }
      }
    },
    test("EvaluationContext with targetingKey and mixed attributes round-trips") {
      val ctxGen = for {
        tk <- Gen.option(nonEmptyString)
        b  <- Gen.boolean
        s  <- nonEmptyString
        i  <- Gen.int
      } yield EvaluationContext(
        targetingKey = tk,
        attributes = Map(
          "bool"   -> AttributeValue.BoolValue(b),
          "string" -> AttributeValue.StringValue(s),
          "int"    -> AttributeValue.IntValue(i)
        )
      )
      check(ctxGen) { ctx =>
        val rt = ContextConverter.fromOpenFeature(ContextConverter.toOpenFeature(ctx))
        assertTrue(
          rt.targetingKey == ctx.targetingKey,
          rt.attributes("bool").asBoolean == ctx.attributes("bool").asBoolean,
          rt.attributes("string").asString == ctx.attributes("string").asString,
          rt.attributes("int").asInt == ctx.attributes("int").asInt
        )
      }
    }
  ) @@ TestAspect.samples(200)
}
