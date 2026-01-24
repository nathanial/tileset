/-
  Tileset/Retry.lean - Retry Logic (DEPRECATED)

  This module re-exports Reactive.RetryConfig and Reactive.RetryState.
  Prefer importing Reactive.Core.Retry directly.

  Migration guide:
  - `Tileset.Retry.RetryConfig` → `Reactive.RetryConfig`
  - `Tileset.Retry.RetryState` → `Reactive.RetryState`
  - `Retry.defaultRetryConfig` → `RetryConfig.default`
  - `rs.backoffDelay config` → `rs.backoffDelayMs config` (now in milliseconds)
-/
import Reactive.Core.Retry

namespace Tileset.Retry

/-- Deprecated: Use Reactive.RetryConfig instead -/
abbrev RetryConfig := Reactive.RetryConfig

/-- Deprecated: Use Reactive.RetryConfig.default instead -/
def defaultRetryConfig : RetryConfig := Reactive.RetryConfig.default

/-- Deprecated: Use Reactive.RetryState instead -/
abbrev RetryState := Reactive.RetryState

end Tileset.Retry
