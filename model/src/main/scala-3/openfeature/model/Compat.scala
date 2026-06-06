package openfeature.model

object Compat:
  type OrError[+E1, +E2] = E1 | E2
