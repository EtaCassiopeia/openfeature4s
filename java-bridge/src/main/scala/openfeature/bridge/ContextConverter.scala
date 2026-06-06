package openfeature.bridge

import openfeature.model.{AttributeValue, EvaluationContext}
import dev.openfeature.sdk.{EvaluationContext => OFEvaluationContext, MutableContext, Structure, Value}

import scala.jdk.CollectionConverters._

/** Internal utility for converting between openfeature.model and OpenFeature Java SDK evaluation contexts.
  *
  * Not part of the user-facing API — used by the zio, cats, and pure backend modules.
  */
object ContextConverter {

  def toOpenFeature(ctx: EvaluationContext): OFEvaluationContext = {
    val mutableCtx = new MutableContext()
    ctx.targetingKey.foreach(key => mutableCtx.setTargetingKey(key))
    ctx.attributes.foreach { case (key, value) => addAttributeToContext(mutableCtx, key, value) }
    mutableCtx
  }

  def fromOpenFeature(ctx: OFEvaluationContext): EvaluationContext = {
    val targetingKey = Option(ctx.getTargetingKey)
    val attributes   = ctx.asMap().asScala.map { case (k, v) => k -> valueToAttribute(v) }.toMap
    EvaluationContext(targetingKey, attributes)
  }

  private def addAttributeToContext(ctx: MutableContext, key: String, attr: AttributeValue): Unit =
    attr match {
      case AttributeValue.BoolValue(b)     => ctx.add(key, b)
      case AttributeValue.StringValue(s)   => ctx.add(key, s)
      case AttributeValue.IntValue(i)      => ctx.add(key, Integer.valueOf(i))
      case AttributeValue.LongValue(l)     => ctx.add(key, java.lang.Double.valueOf(l.toDouble))
      case AttributeValue.DoubleValue(d)   => ctx.add(key, d)
      case AttributeValue.InstantValue(dt) => ctx.add(key, dt)
      case AttributeValue.ListValue(list) =>
        ctx.add(key, list.map(attributeToValue).asJava)
      case AttributeValue.StructValue(map) =>
        val javaMap: java.util.Map[String, Object] =
          map.map { case (k, v) => k -> attributeToValue(v).asObject() }.asJava
        ctx.add(key, Structure.mapToStructure(javaMap))
    }

  private def attributeToValue(attr: AttributeValue): Value = attr match {
    case AttributeValue.BoolValue(b)     => new Value(b)
    case AttributeValue.StringValue(s)   => new Value(s)
    case AttributeValue.IntValue(i)      => new Value(i)
    case AttributeValue.LongValue(l)     => new Value(l.toDouble)
    case AttributeValue.DoubleValue(d)   => new Value(d)
    case AttributeValue.InstantValue(dt) => new Value(dt.toString)
    case AttributeValue.ListValue(list) =>
      new Value(list.map(attributeToValue).asJava)
    case AttributeValue.StructValue(map) =>
      val javaMap: java.util.Map[String, Object] =
        map.map { case (k, v) => k -> attributeToValue(v).asObject() }.asJava
      new Value(Structure.mapToStructure(javaMap))
  }

  private def valueToAttribute(value: Value): AttributeValue =
    if (value.isBoolean) AttributeValue.BoolValue(value.asBoolean())
    else if (value.isString) AttributeValue.StringValue(value.asString())
    else if (value.isNumber) {
      val num = value.asDouble()
      // Long.MaxValue.toDouble rounds up past Long.MaxValue, so use strict < to avoid saturation.
      if (num == num.toLong.toDouble && num >= Long.MinValue.toDouble && num < Long.MaxValue.toDouble) {
        val asLong = num.toLong
        if (asLong >= Int.MinValue && asLong <= Int.MaxValue) AttributeValue.IntValue(asLong.toInt)
        else AttributeValue.LongValue(asLong)
      } else AttributeValue.DoubleValue(num)
    } else if (value.isList) {
      AttributeValue.ListValue(value.asList().asScala.map(valueToAttribute).toList)
    } else if (value.isStructure) {
      val struct = value.asStructure().asMap().asScala.map { case (k, v) => k -> valueToAttribute(v) }.toMap
      AttributeValue.StructValue(struct)
    } else if (value.isInstant) AttributeValue.InstantValue(value.asInstant())
    else AttributeValue.StringValue(value.asString())
}
