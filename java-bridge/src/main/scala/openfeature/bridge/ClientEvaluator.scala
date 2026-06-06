package openfeature.bridge

import dev.openfeature.sdk.{Client => OFClient, EvaluationContext => OFEvaluationContext, FlagEvaluationDetails}

/** Typeclass for type-safe dispatch to the OpenFeature Java SDK client methods.
  *
  * Each instance knows how to call the correct SDK method (getBooleanDetails, getStringDetails, etc.) for a given Scala
  * type and how to extract the Scala-typed value from the raw SDK result.
  *
  * The `evaluate` method is a plain blocking call — effect-system wrappers (ZIO.attemptBlocking, IO.blocking, etc.) are
  * applied by each backend module, not here.
  *
  * Not part of the user-facing API.
  */
trait ClientEvaluator[A] {

  /** Call the appropriate typed SDK method. Blocking; may throw. */
  def evaluate(
    client: OFClient,
    key: String,
    default: A,
    context: OFEvaluationContext
  ): FlagEvaluationDetails[_]

  /** Extract the Scala-typed value from the raw SDK evaluation details. */
  def extractValue(details: FlagEvaluationDetails[_]): A
}

object ClientEvaluator {

  /** Type-erased evaluation bundle produced by [[evaluateStandard]].
    *
    * `call` is the thunk to invoke (blocking, may throw). `extract` converts the raw SDK details to the target type A.
    * The backend module wraps `call` in its own blocking-effect combinator.
    */
  final case class Erased[A](
    call: () => FlagEvaluationDetails[_],
    extract: FlagEvaluationDetails[_] => A
  )

  /** Look up the evaluator for a standard type by name and produce a type-erased evaluation bundle.
    *
    * Returns `None` for non-standard types (Object, custom) which require special handling in the backend module.
    */
  def evaluateStandard[A](
    typeName: String,
    client: OFClient,
    key: String,
    default: A,
    context: OFEvaluationContext
  ): Option[Erased[A]] = {
    def erased[T](ev: ClientEvaluator[T]): Erased[A] =
      Erased[A](
        call = () => ev.evaluate(client, key, default.asInstanceOf[T], context),
        extract = details => ev.extractValue(details).asInstanceOf[A]
      )
    typeName match {
      case "Boolean" => Some(erased(booleanEvaluator))
      case "String"  => Some(erased(stringEvaluator))
      case "Int"     => Some(erased(intEvaluator))
      case "Long"    => Some(erased(longEvaluator))
      case "Float"   => Some(erased(floatEvaluator))
      case "Double"  => Some(erased(doubleEvaluator))
      case _         => None
    }
  }

  implicit val booleanEvaluator: ClientEvaluator[Boolean] = new ClientEvaluator[Boolean] {
    def evaluate(
      client: OFClient,
      key: String,
      default: Boolean,
      context: OFEvaluationContext
    ): FlagEvaluationDetails[_] =
      client.getBooleanDetails(key, default, context)

    def extractValue(details: FlagEvaluationDetails[_]): Boolean =
      details.getValue.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  implicit val stringEvaluator: ClientEvaluator[String] = new ClientEvaluator[String] {
    def evaluate(
      client: OFClient,
      key: String,
      default: String,
      context: OFEvaluationContext
    ): FlagEvaluationDetails[_] =
      client.getStringDetails(key, default, context)

    def extractValue(details: FlagEvaluationDetails[_]): String =
      details.getValue.asInstanceOf[String]
  }

  implicit val intEvaluator: ClientEvaluator[Int] = new ClientEvaluator[Int] {
    def evaluate(client: OFClient, key: String, default: Int, context: OFEvaluationContext): FlagEvaluationDetails[_] =
      client.getIntegerDetails(key, Integer.valueOf(default), context)

    def extractValue(details: FlagEvaluationDetails[_]): Int =
      details.getValue.asInstanceOf[java.lang.Integer].intValue()
  }

  // Long uses the Double SDK method — exact for integers up to 2^53.
  implicit val longEvaluator: ClientEvaluator[Long] = new ClientEvaluator[Long] {
    def evaluate(client: OFClient, key: String, default: Long, context: OFEvaluationContext): FlagEvaluationDetails[_] =
      client.getDoubleDetails(key, java.lang.Double.valueOf(default.toDouble), context)

    def extractValue(details: FlagEvaluationDetails[_]): Long =
      details.getValue.asInstanceOf[java.lang.Double].longValue()
  }

  // Float uses the Double SDK method with conversion.
  implicit val floatEvaluator: ClientEvaluator[Float] = new ClientEvaluator[Float] {
    def evaluate(
      client: OFClient,
      key: String,
      default: Float,
      context: OFEvaluationContext
    ): FlagEvaluationDetails[_] =
      client.getDoubleDetails(key, java.lang.Double.valueOf(default.toDouble), context)

    def extractValue(details: FlagEvaluationDetails[_]): Float =
      details.getValue.asInstanceOf[java.lang.Double].floatValue()
  }

  implicit val doubleEvaluator: ClientEvaluator[Double] = new ClientEvaluator[Double] {
    def evaluate(
      client: OFClient,
      key: String,
      default: Double,
      context: OFEvaluationContext
    ): FlagEvaluationDetails[_] =
      client.getDoubleDetails(key, java.lang.Double.valueOf(default), context)

    def extractValue(details: FlagEvaluationDetails[_]): Double =
      details.getValue.asInstanceOf[java.lang.Double].doubleValue()
  }
}
