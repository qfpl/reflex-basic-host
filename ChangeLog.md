# ChangeLog

## 0.2 - 2019-08-29

* Added `Reflex.Host.Basic.repeatUntilQuit_`, with the same behaviour
  as 0.1's `Reflex.Host.Basic.repeatUntilQuit`.
* `Reflex.Host.Basic.repeatUntilQuit` now returns an `Event` that
  fires each time the action executes. If you don't need this,
  consider `Reflex.Host.Basic.repeatUntilQuit_`.
* `Reflex.Host.Basic.basicHostWithQuit`: Expect a guest that returns
  only `Event t ()`, as trying to return an actual result gives
  hard-to-diagnose type errors at the use site and most people
  returned `()` anyway.
* `Reflex.Host.Basic.basicHostForever`: Return `()` instead of `a` for
  the same reason.
* Do not fork a new thread when starting the host.
* All hosts now run in separate reflex timelines.
* Add example of a program with two independent hosts.

## 0.1 - 2019-03-19

* Initial release
