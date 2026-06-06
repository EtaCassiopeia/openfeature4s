import xerial.sbt.Sonatype.sonatypeCentralHost

val scala213Version       = "2.13.16"
val scala3Version         = "3.3.4"
val zioVersion            = "2.1.14"
val openFeatureSdkVersion = "1.20.2"
val catsEffectVersion     = "3.5.4"
val fs2Version            = "3.10.2"

// OpenFeature Specification Compatibility
// Spec version: v0.8.0 (https://github.com/open-feature/spec)
// This library implements the dynamic-context (server-side) paradigm

ThisBuild / scalaVersion       := scala3Version
ThisBuild / crossScalaVersions := Seq(scala213Version, scala3Version)
ThisBuild / organization       := "io.github.etacassiopeia"

ThisBuild / homepage := Some(url("https://github.com/EtaCassiopeia/openfeature4s"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer(
    id = "EtaCassiopeia",
    name = "Mohsen Zainalpour",
    email = "zainalpour@gmail.com",
    url = url("https://github.com/EtaCassiopeia")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/EtaCassiopeia/openfeature4s"),
    "scm:git:git@github.com:EtaCassiopeia/openfeature4s.git"
  )
)

ThisBuild / sonatypeCredentialHost := sonatypeCentralHost
ThisBuild / versionScheme          := Some("semver-spec")

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds"
)

ThisBuild / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq("-Xsource:3", "-Wconf:cat=scala3-migration:w")
    case Some((3, _)) => Seq("-Xfatal-warnings", "-Yretain-trees")
    case _            => Seq()
  }
}

ThisBuild / coverageEnabled          := false
ThisBuild / coverageMinimumStmtTotal := 80
ThisBuild / coverageFailOnMinimum    := true

// Binary-compatibility check via sbt-mima. `mimaPreviousArtifacts` is intentionally empty until
// the first post-mima tag exists. See https://github.com/lightbend/mima for the filter API.
ThisBuild / mimaFailOnNoPrevious := false

// Version-specific source directories for cross-compiled modules.
// scala-3/ contains Scala 3-specific syntax (enums, opaque types, given/using).
// scala-2/ contains equivalent Scala 2.13 implementations (sealed traits, implicit, AnyVal).
// Scala 3-only modules (cats, kyo, pure) do NOT use this — all their source lives in src/main/scala/.
lazy val crossVersionSourceDirs = Seq(
  Compile / unmanagedSourceDirectories ++= {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(sourceDir / "scala-2")
      case Some((3, _)) => Seq(sourceDir / "scala-3")
      case _            => Seq()
    }
  },
  Test / unmanagedSourceDirectories ++= {
    val sourceDir = (Test / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(sourceDir / "scala-2")
      case Some((3, _)) => Seq(sourceDir / "scala-3")
      case _            => Seq()
    }
  }
)

// Applied to all modules that use zio-test as the test framework.
lazy val zioTestSettings = Seq(
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-test"     % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test
  )
)

lazy val commonSettings = Seq(
  mimaPreviousArtifacts := Set.empty
)

// Restricts a module to Scala 3 only (no cross-compilation).
lazy val scala3OnlySettings = Seq(
  crossScalaVersions := Seq(scala3Version)
)

// Pure data types — no effect-system dependency.
lazy val model = (project in file("model"))
  .settings(
    name := "openfeature4s-model",
    commonSettings,
    crossVersionSourceDirs
  )

// Internal Java SDK interop glue — not user-facing API.
lazy val javaBridge = (project in file("java-bridge"))
  .dependsOn(model)
  .settings(
    name := "openfeature4s-java-bridge",
    commonSettings,
    crossVersionSourceDirs,
    libraryDependencies += "dev.openfeature" % "sdk" % openFeatureSdkVersion
  )

// ZIO 2 backend — port of zio-openfeature/core.
lazy val zio = (project in file("zio"))
  .dependsOn(javaBridge)
  .settings(
    name := "openfeature4s-zio",
    commonSettings,
    zioTestSettings,
    crossVersionSourceDirs,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion
    )
  )

// Testing utilities for ZIO backend users.
lazy val zioTestkit = (project in file("zio-testkit"))
  .dependsOn(zio)
  .settings(
    name := "openfeature4s-zio-testkit",
    commonSettings,
    zioTestSettings,
    crossVersionSourceDirs,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"      % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion
    )
  )

// Built-in providers: HOCON, env-var, caching wrapper.
lazy val zioExtras = (project in file("zio-extras"))
  .dependsOn(zio)
  .settings(
    name := "openfeature4s-zio-extras",
    commonSettings,
    zioTestSettings,
    crossVersionSourceDirs,
    libraryDependencies ++= Seq(
      "dev.zio"      %% "zio"       % zioVersion,
      "dev.zio"      %% "zio-cache" % "0.2.3",
      "com.typesafe"  % "config"    % "1.4.3"
    )
  )

// OpenFeature Remote Evaluation Protocol provider.
// Kept separate from zio-extras to avoid pulling in OFREP's HTTP-client transitive stack.
lazy val zioOfrep = (project in file("zio-ofrep"))
  .dependsOn(zio)
  .settings(
    name := "openfeature4s-zio-ofrep",
    commonSettings,
    zioTestSettings,
    crossVersionSourceDirs,
    libraryDependencies ++= Seq(
      "dev.zio"                           %% "zio"   % zioVersion,
      "dev.openfeature.contrib.providers"  % "ofrep" % "0.0.1"
    ),
    dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-core" % "2.21.2"
  )

// Optimizely provider via Optimizely Java SDK.
lazy val zioOptimizely = (project in file("zio-optimizely"))
  .dependsOn(zio)
  .settings(
    name := "openfeature4s-zio-optimizely",
    commonSettings,
    zioTestSettings,
    crossVersionSourceDirs,
    libraryDependencies ++= Seq(
      "dev.zio"          %% "zio"                  % zioVersion,
      "com.optimizely.ab" % "core-api"             % "4.2.2",
      "com.optimizely.ab" % "core-httpclient-impl" % "4.2.2"
    )
  )

// Cats Effect 3 + FS2 backend (Scala 3 only).
lazy val cats = (project in file("cats"))
  .dependsOn(javaBridge)
  .settings(
    name := "openfeature4s-cats",
    commonSettings,
    scala3OnlySettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"       % catsEffectVersion,
      "co.fs2"        %% "fs2-core"          % fs2Version,
      "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

// Testing utilities for Cats Effect backend users (Scala 3 only).
lazy val catsTestkit = (project in file("cats-testkit"))
  .dependsOn(cats)
  .settings(
    name := "openfeature4s-cats-testkit",
    commonSettings,
    scala3OnlySettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    )
  )

// Kyo backend — experimental, Scala 3 only. No Java SDK bridge dependency.
// kyo-core version: update when stabilized (see issue #9).
lazy val kyo = (project in file("kyo"))
  .dependsOn(model)
  .settings(
    name := "openfeature4s-kyo",
    commonSettings,
    scala3OnlySettings
    // libraryDependencies += "io.getkyo" %% "kyo-core" % "<version>" — added in issue #9
  )

// Synchronous Either-based backend, no async runtime (Scala 3 only).
lazy val pure = (project in file("pure"))
  .dependsOn(javaBridge)
  .settings(
    name := "openfeature4s-pure",
    commonSettings,
    scala3OnlySettings
  )

lazy val root = (project in file("."))
  .aggregate(
    model,
    javaBridge,
    zio, zioTestkit, zioExtras, zioOfrep, zioOptimizely,
    cats, catsTestkit,
    kyo,
    pure
  )
  .settings(
    name          := "openfeature4s",
    publish / skip := true
  )
