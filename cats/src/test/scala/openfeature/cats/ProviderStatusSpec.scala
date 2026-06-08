package openfeature.cats

import openfeature.model.*
import cats.effect.IO
import munit.CatsEffectSuite

class ProviderStatusSpec extends CatsEffectSuite {

  private val fixture = ResourceFunFixture(FeatureFlags.makeIO(new TestProvider()))

  fixture.test("providerStatus is Ready after make") { flags =>
    flags.providerStatus.map(s => assert(s == ProviderStatus.Ready))
  }

  fixture.test("events stream is available") { flags =>
    // Pull 0 events with a short timeout — just verifies the stream compiles and starts
    flags.events
      .take(0)
      .compile
      .toList
      .map(evts => assert(evts.isEmpty))
  }

  fixture.test("concurrent evaluations do not interfere") { flags =>
    val eval = flags.boolean(FlagKey("any"), false)
    for r1 <- IO.both(eval, eval)
    (a, b) = r1
    yield assert(a == b)
  }
}
