# RTNetlink

RTNetlink is an extensible, high-level, pure Haskell interface for manipulating
network interfaces on Linux: creating and destroying interfaces, changing and
dumping interface settings, adding and removing addresses.

The core interface of RTNetlink is the `RTNL` monad. `RTNL` handles the heavy
lifting of opening and closing netlink sockets, incrementing sequence numbers,
and getting the responses for the current sequence number behind the scenes.
Messages not that are not responses to a sent message, such as those sent to
group subscribers, are stored in the backlog and can be retrieved with
'getBacklog'.

The basic way to use `RTNL` is to use the `create`, `destroy`, `dump`, and
`change` convenience functions. If you want more control, you can use 'talk'
and `talk_`. Import modules like `System.Linux.RTNetlink.Link` to get access
to prefab instances of `Create` and `Destroy` messages, etc. Or import
`System.Linux.RTNetlink.Message` to get access to the core typeclasses and
create your own messages. `System.Linux.RTNetlink.Packet` has a number of
functions to make this easier.

## Example:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Link
import Control.Monad (when)

main :: IO ()
main = runRTNL $ do
    let mybridge = LinkName "mybridge"
    create (bridge mybridge)
    change mybridge Up
    state <- dump mybridge
    when (head state == Up) $
        liftIO (putStrLn "I did it, mom!")
    destroy mybridge
```
