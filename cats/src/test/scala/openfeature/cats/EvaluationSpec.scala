package openfeature.cats

import openfeature.model.*
import cats.effect.IO
import munit.CatsEffectSuite

/** Core flag evaluation tests — typed evaluation, defaults, and error handling. */
class EvaluationSpec extends CatsEffectSuite:

  private val fixture = ResourceFunFixture(
    FeatureFlags.makeIO(
      new TestProvider(
        Map(
          "bool-flag"   -> true,
          "string-flag" -> "hello",
          "int-flag"    -> 42,
          "long-flag"   -> (Int.MaxValue.toLong + 1L),
          "double-flag" -> 2.718
        )
      )
    )
  )

  fixture.test("boolean flag returns configured value") { flags =>
    flags.boolean(FlagKey("bool-flag"), false).map(v => assert(v == true))
  }

  fixture.test("boolean flag returns default when not set") { flags =>
    flags.boolean(FlagKey("unknown"), false).map(v => assert(v == false))
  }

  fixture.test("string flag returns configured value") { flags =>
    flags.string(FlagKey("string-flag"), "").map(v => assert(v == "hello"))
  }

  fixture.test("int flag returns configured value") { flags =>
    flags.int(FlagKey("int-flag"), 0).map(v => assert(v == 42))
  }

  fixture.test("double flag returns configured value") { flags =>
    flags.double(FlagKey("double-flag"), 0.0).map(v => assert(v > 2.7 && v < 2.8))
  }

  fixture.test("booleanDetails returns FlagResolution with reason") { flags =>
    flags.booleanDetails(FlagKey("bool-flag"), false).map { res =>
      assert(res.value == true)
      assert(res.reason == ResolutionReason.TargetingMatch)
    }
  }

  fixture.test("booleanDetails for missing flag returns Default reason") { flags =>
    flags.booleanDetails(FlagKey("no-such-flag"), false).map { res =>
      assert(res.value == false)
      assert(res.reason == ResolutionReason.Default)
    }
  }

  fixture.test("stringDetails carries flagKey") { flags =>
    flags.stringDetails(FlagKey("string-flag"), "").map { res =>
      assert(res.flagKey == "string-flag")
    }
  }
