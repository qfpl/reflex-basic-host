# THIS LIBRARY IS DEPRECATED

Since `reflex-0.7.1.0`, there is a
[`Reflex.Host.Headless`](https://hackage.haskell.org/package/reflex-0.7.1.0/docs/Reflex-Host-Headless.html)
module in mainline Reflex, which allows you to do the same things as
`reflex-basic-host`.

# reflex-basic-host

[![Build Status](https://travis-ci.org/qfpl/reflex-basic-host.svg?branch=master)](https://travis-ci.org/qfpl/reflex-basic-host)

A basic `reflex` host for backend work.

## Getting Started

The main entry point is `Reflex.Host.Basic.basicHostWithQuit`. The
`BasicGuest` type that it expects has instances for most of the
important Reflex typeclasses. To create your own `Event`s, use
functions from by `Reflex.TriggerEvent`. To peform side-effects, use
functions from `Reflex.PerformEvent` - `Performable (BasicGuest t m)`
has a `MonadIO` instance.

## Examples

For some usage examples, see [the example
directory](https://github.com/qfpl/reflex-basic-host/tree/master/example)

## Contribution

Feel free to file an issue or pull request on Github, or contact us at:

* IRC - #qfpl on Freenode
* Email - <oᴉ˙ldɟb@llǝʞsɐɥ>
