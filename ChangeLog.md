# Revision history for rtnetlink

## 0.2.0.0  -- 2018-01-01

* According to rtnetlink(7), `ifi_change` in `struct ifinfomsg` "is reserved
  for future use and should be always set to `0xFFFFFFFF`", so did that.
* Added a new typeclass, `Dump`, that encodes request-reply pairs that can be
  used with `dump`.
* Added `Integral` and `IsString` instances for appropriate `newtype`s.

## 0.1.0.4  -- 2017-03-28

* Removed some obscure rtnetlink groups that aren't in `linux/rtnetlink.h` on
  Trusty.
* Added `stack.yaml`.

## 0.1.0.3  -- 2017-03-27

* Provided support for `base-4.8.*` by fixing `IsString` ambiguity in
  `LinkEther`.

## 0.1.0.2  -- 2017-03-26

* Provided support for `transformers-0.4.*`.

## 0.1.0.1  -- 2017-03-25

* Provided support for `base-4.7.*`.

## 0.1.0.0  -- 2017-03-24

* First version. Released on an unsuspecting world.
