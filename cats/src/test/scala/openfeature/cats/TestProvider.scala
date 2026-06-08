package openfeature.cats

import dev.openfeature.sdk.*

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Minimal in-memory provider for cats compliance tests. */
final class TestProvider(initialFlags: Map[String, Any] = Map.empty) extends FeatureProvider {
  private val flags = new ConcurrentHashMap[String, Any]()
  private val state = new AtomicReference[ProviderState](ProviderState.READY)

  initialFlags.foreach { case (k, v) => flags.put(k, v) }

  def setFlag(key: String, value: Any): Unit = flags.put(key, value)
  def removeFlag(key: String): Unit          = flags.remove(key)
  def clearFlags(): Unit                     = flags.clear()

  @scala.annotation.nowarn("msg=deprecated")
  override def getMetadata: Metadata = new Metadata { def getName: String = "TestProvider" }

  override def getState: ProviderState = state.get()

  override def initialize(ctx: EvaluationContext): Unit = state.set(ProviderState.READY)
  override def shutdown(): Unit                         = state.set(ProviderState.NOT_READY)

  override def getBooleanEvaluation(key: String, default: java.lang.Boolean, ctx: EvaluationContext) =
    eval(key, default, _.asInstanceOf[Boolean])

  override def getStringEvaluation(key: String, default: String, ctx: EvaluationContext) =
    eval(key, default, _.toString)

  override def getIntegerEvaluation(key: String, default: java.lang.Integer, ctx: EvaluationContext) =
    eval(key, default, { case i: Int => i; case n: Number => n.intValue() })

  override def getDoubleEvaluation(key: String, default: java.lang.Double, ctx: EvaluationContext) =
    eval(key, default, { case d: Double => d; case n: Number => n.doubleValue() })

  override def getObjectEvaluation(key: String, default: Value, ctx: EvaluationContext) =
    eval[Value](key, default, v => new Value(v.toString))

  private def eval[A](key: String, default: A, convert: Any => A): ProviderEvaluation[A] =
    Option(flags.get(key)) match {
      case Some(v) =>
        ProviderEvaluation
          .builder[A]()
          .value(convert(v))
          .reason("TARGETING_MATCH")
          .build()
      case None =>
        ProviderEvaluation
          .builder[A]()
          .value(default)
          .reason("DEFAULT")
          .build()
    }
}
