# changelog

## 0.2

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
