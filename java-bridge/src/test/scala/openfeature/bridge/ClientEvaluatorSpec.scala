package openfeature.bridge

import dev.openfeature.sdk.FlagEvaluationDetails
import zio.test._

object ClientEvaluatorSpec extends ZIOSpecDefault {

  private def makeDetails[A](value: A): FlagEvaluationDetails[A] = {
    val details = new FlagEvaluationDetails[A]()
    details.setValue(value)
    details.setFlagKey("test-flag")
    details
  }

  def spec = suite("ClientEvaluator")(
    suite("Boolean")(
      test("extractValue converts Java Boolean to Scala Boolean") {
        val result =
          ClientEvaluator.booleanEvaluator.extractValue(makeDetails[java.lang.Boolean](java.lang.Boolean.TRUE))
        assertTrue(result == true)
      },
      test("extractValue handles false") {
        val result =
          ClientEvaluator.booleanEvaluator.extractValue(makeDetails[java.lang.Boolean](java.lang.Boolean.FALSE))
        assertTrue(result == false)
      }
    ),
    suite("String")(
      test("extractValue returns string value") {
        val result = ClientEvaluator.stringEvaluator.extractValue(makeDetails[String]("hello"))
        assertTrue(result == "hello")
      },
      test("extractValue handles empty string") {
        val result = ClientEvaluator.stringEvaluator.extractValue(makeDetails[String](""))
        assertTrue(result == "")
      }
    ),
    suite("Int")(
      test("extractValue converts Java Integer to Scala Int") {
        val result = ClientEvaluator.intEvaluator.extractValue(makeDetails[java.lang.Integer](Integer.valueOf(42)))
        assertTrue(result == 42)
      },
      test("extractValue handles negative values") {
        val result = ClientEvaluator.intEvaluator.extractValue(makeDetails[java.lang.Integer](Integer.valueOf(-1)))
        assertTrue(result == -1)
      }
    ),
    suite("Long")(
      test("extractValue converts Java Double to Scala Long") {
        val result =
          ClientEvaluator.longEvaluator.extractValue(makeDetails[java.lang.Double](java.lang.Double.valueOf(42.0)))
        assertTrue(result == 42L)
      },
      test("extractValue handles values larger than Int.MaxValue") {
        val large = Int.MaxValue.toLong + 1000L
        val result = ClientEvaluator.longEvaluator.extractValue(
          makeDetails[java.lang.Double](java.lang.Double.valueOf(large.toDouble))
        )
        assertTrue(result == large)
      }
    ),
    suite("Float")(
      test("extractValue converts Java Double to Scala Float") {
        val result =
          ClientEvaluator.floatEvaluator.extractValue(makeDetails[java.lang.Double](java.lang.Double.valueOf(3.14)))
        assertTrue(result == 3.14.toFloat)
      },
      test("extractValue handles zero") {
        val result =
          ClientEvaluator.floatEvaluator.extractValue(makeDetails[java.lang.Double](java.lang.Double.valueOf(0.0)))
        assertTrue(result == 0.0f)
      }
    ),
    suite("Double")(
      test("extractValue converts Java Double to Scala Double") {
        val result =
          ClientEvaluator.doubleEvaluator.extractValue(makeDetails[java.lang.Double](java.lang.Double.valueOf(3.14)))
        assertTrue(result == 3.14)
      },
      test("extractValue handles negative values") {
        val result =
          ClientEvaluator.doubleEvaluator.extractValue(makeDetails[java.lang.Double](java.lang.Double.valueOf(-1.5)))
        assertTrue(result == -1.5)
      }
    ),
    suite("implicit resolution")(
      test("all standard instances resolve") {
        assertTrue(implicitly[ClientEvaluator[Boolean]] != null) &&
        assertTrue(implicitly[ClientEvaluator[String]] != null) &&
        assertTrue(implicitly[ClientEvaluator[Int]] != null) &&
        assertTrue(implicitly[ClientEvaluator[Long]] != null) &&
        assertTrue(implicitly[ClientEvaluator[Float]] != null) &&
        assertTrue(implicitly[ClientEvaluator[Double]] != null)
      }
    ),
    suite("evaluateStandard")(
      test("returns Some for all standard type names") {
        val client  = null.asInstanceOf[dev.openfeature.sdk.Client]
        val context = null.asInstanceOf[dev.openfeature.sdk.EvaluationContext]
        assertTrue(ClientEvaluator.evaluateStandard("Boolean", client, "k", false, context).isDefined) &&
        assertTrue(ClientEvaluator.evaluateStandard("String", client, "k", "", context).isDefined) &&
        assertTrue(ClientEvaluator.evaluateStandard("Int", client, "k", 0, context).isDefined) &&
        assertTrue(ClientEvaluator.evaluateStandard("Long", client, "k", 0L, context).isDefined) &&
        assertTrue(ClientEvaluator.evaluateStandard("Float", client, "k", 0f, context).isDefined) &&
        assertTrue(ClientEvaluator.evaluateStandard("Double", client, "k", 0.0, context).isDefined)
      },
      test("returns None for non-standard type names") {
        val client  = null.asInstanceOf[dev.openfeature.sdk.Client]
        val context = null.asInstanceOf[dev.openfeature.sdk.EvaluationContext]
        assertTrue(ClientEvaluator.evaluateStandard("Object", client, "k", Map.empty[String, Any], context).isEmpty) &&
        assertTrue(ClientEvaluator.evaluateStandard("Custom", client, "k", "", context).isEmpty)
      }
    )
  )
}
