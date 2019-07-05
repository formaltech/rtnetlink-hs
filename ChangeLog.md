# Revision history for rtnetlink

## 0.2.0.0  -- 2019-07-04

* According to rtnetlink(7), `ifi_change` in `struct ifinfomsg` "is reserved
  for future use and should be always set to `0xFFFFFFFF`", so did that.
* Added a new typeclass, `Dump`, that encodes request-reply pairs that can be
  used with `dump`.
* Added `Integral` and `IsString` instances for appropriate `newtype`s.
* Added general `LinkType` for creating links, instead of creating a new type
  for each kind of link.
* Added several new types to `Address.hs`, including ones for interface scope,
  precedence, labels, lifetimes, and IPv6 features.
* Added several new types to `Link.hs`, including ones for bridge slaves, link
  groups, and link stats.
* Added support for creating and managing vlan interfaces.
* Removed redundancies in `Create`, `Destroy`, `Change`, and `Dump`
  typeclasses, as well as header classes. This should drastically improve the
  DRYness of adding features.
* Fixed a bug that prevented nested `struct nlattr`s from being parsed.
* Made many minor interface improvements.

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
