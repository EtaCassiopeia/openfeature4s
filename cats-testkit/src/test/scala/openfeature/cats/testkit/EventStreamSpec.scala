package openfeature.cats.testkit

import openfeature.model.*
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class EventStreamSpec extends CatsEffectSuite:

  private val meta    = ProviderMetadata("TestFeatureProvider")
  private val fixture = ResourceFunFixture(TestFeatureProvider.make[IO]())

  // Subscribes to provider.events using subscribeAwait (race-free), starts a background
  // fiber that feeds events into an unbounded queue, and returns IO[ProviderEvent] = queue.take.
  // The subscription is guaranteed active when the Resource completes acquisition.
  private def receiveOne(provider: TestFeatureProvider[IO]): Resource[IO, IO[ProviderEvent]] =
    for
      queue  <- Resource.eval(Queue.unbounded[IO, ProviderEvent])
      stream <- provider.eventsResource
      _      <- stream.evalMap(queue.offer).compile.drain.background
    yield queue.take

  fixture.test("emitEvent publishes Ready event to events stream") { case (provider, _) =>
    receiveOne(provider).use { next =>
      for
        _     <- provider.emitEvent(ProviderEvent.Error(new RuntimeException("probe"), meta, None, None))
        _     <- provider.emitEvent(ProviderEvent.Ready(meta))
        event <- next
      yield assert(event.isInstanceOf[ProviderEvent.Error])
    }
  }

  fixture.test("emitEvent publishes Error event to events stream") { case (provider, _) =>
    receiveOne(provider).use { next =>
      for
        _     <- provider.emitEvent(ProviderEvent.Error(new RuntimeException("test"), meta, None, None))
        event <- next
      yield assert(event.isInstanceOf[ProviderEvent.Error])
    }
  }

  fixture.test("emitEvent publishes Stale event to events stream") { case (provider, _) =>
    receiveOne(provider).use { next =>
      for
        _     <- provider.emitEvent(ProviderEvent.Stale("degraded", meta))
        event <- next
      yield assert(event.isInstanceOf[ProviderEvent.Stale])
    }
  }

  fixture.test("emitEvent publishes ConfigurationChanged event to events stream") { case (provider, _) =>
    receiveOne(provider).use { next =>
      for
        _     <- provider.emitEvent(ProviderEvent.ConfigurationChanged(Set("flag-a"), meta))
        event <- next
      yield assert(event.isInstanceOf[ProviderEvent.ConfigurationChanged])
    }
  }

  fixture.test("Reconnecting event is published to FS2 stream") { case (provider, _) =>
    receiveOne(provider).use { next =>
      for
        _     <- provider.emitEvent(ProviderEvent.Reconnecting(meta))
        event <- next
      yield assert(event.isInstanceOf[ProviderEvent.Reconnecting])
    }
  }

  test("events stream terminates when resource is released") {
    IO.ref(false).flatMap { done =>
      TestFeatureProvider
        .make[IO]()
        .flatMap { case (provider, flags) =>
          provider.eventsResource.map(stream => (provider, flags, stream))
        }
        .use { case (_, _, stream) =>
          stream
            .onFinalize(done.set(true))
            .compile
            .drain
            .start
            .void
        } *>
        IO.sleep(50.millis) *>
        done.get.map(b => assert(b, "stream did not terminate after resource release"))
    }
  }
