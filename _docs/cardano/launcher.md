---
layout: default
title: Cardano SL Launcher
permalink: /cardano/launcher/
group: cardano
visible: true
---

# Cardano SL Launcher

## Overview

An executable [`cardano-launcher`](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/cardano-sl.cabal#L1074) is a tool for launching Cardano SL. You can run it with `--help` flag to see this usage info:

~~~
$ cardano-launcher --help
Usage: cardano-launcher --node PATH [-n ARG] [--node-log PATH] [--wallet PATH]
                        [-w ARG] --updater PATH [-u ARG] [--update-archive PATH]
                        --node-timeout SEC [--report-server URL]
  Tool to launch Cardano SL

Available options:
  -h,--help                Show this help text
  --node PATH              Path to the node executable
  -n ARG                   An argument to be passed to the node
  --node-log PATH          Path to the log where node's output will be dumped
  --wallet PATH            Path to the wallet executable
  -w ARG                   An argument to be passed to the wallet
  --updater PATH           Path to the updater executable
  -u ARG                   An argument to be passed to the updater
  --update-archive PATH    Path to the update archive (will be passed to the
                           updater)
  --node-timeout SEC       How much to wait for the node to exit before killing
                           it
  --report-server URL      Where to send logs in case of failure
~~~

For example (run via `stack`):

~~~
$ stack exec cardano-launcher --                     \
    --node /tmp/blah-v000/node                       \
    --node-timeout 5                                 \
    --updater "/bin/cardano-updater /tmp/n/ /tmp/n/" \
    --update-archive /tmp/cardano-update.tar
~~~

`cardano-node` is a [node application](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/cardano-sl.cabal#L572). `cardano-updater` is a [separate application](https://github.com/input-output-hk/cardano-updater) for the node updates, see an explanation [below](#updater). Node timeout is explaned [below](#desktop-scenario).

## Scenarios

There are two work scenarios for `cardano-launcher`: desktop and server. If you provide a path to the [wallet executable](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/cardano-sl.cabal#L677) using `--wallet` argument during start, `cardano-node` will run [in desktop](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L108) scenario, otherwise [in server](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L103) one.

### Server Scenario

There are 2 steps after start:

1. run an updater,
2. run a node.

Updater's work is explaned [below](#updater).

Node is [spawning as a separate process](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L219). Please note: we expect that node will be [actually started during 5 seconds](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L220) after spawning. If not - we exit with panic.

After that we just [wait until node stopped](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L129). When node exited, we check its exit code. If it's equal to `20`, we [restart the launcher in the server scenario](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L132), otherwise just [quit](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L135).

We can write log info in the log file before quitting. To do it, we must provide two additional arguments during launcher's start, `--report-server` and `--node-log`. First argument defines an URL of the report server, the second one defines a path to the log file. We asynchronously send log info to the report server [via `POST`-request](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L242). Please note that file from the `--node-log` argument must exist.

### Desktop Scenario

There are 3 steps after start:

1. run an updater,
2. run a node,
3. run a wallet.

The first and second steps already described above, in [Server Scenario](#server-scenario).

The wallet is [spawning as a separate process](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L155) too.

After that we just [wait until node or wallet stopped](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L156). When stopped, we check it:

1. If the node exited, we write a log (see explanation above, in [Server Scenario](#server-scenario)) and just [waiting for a wallet's death](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L163).
2. If the wallet exited, we check an exit code, and, if it's equal to `20`, we [kills a node](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L170) and [restart the launcher in the desktop scenario](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L172). Please note that we kill a node not immediately, but after timeout, and its value is obtained from the `--node-timeout` argument mentioned above.
3. If the wallet exited _and_ exit code isn't equal to `20`, we [kills a node](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L178) immediately.

## Updater

Important thing `cardano-launcher` do is a node updating. The first step in both scenarios is an [updater running](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L183). Please note that if executable file in the `--updater` argument doesn't exist, updating will be [skipped](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L187).

The core idea is very simple. Update of the node is a special `.tar`-archive, it represents a _difference_ between old/current version of the node and new version of it. Path to this archive is obtained via `--update-archive` argument. So, `cardano-updater` applies this archive to the node, so after restart we'll use a new version of the node.

Please note that when we run `cardano-launcher` in desktop scenario, wallet's user is able to see that update is here. And there are two possible situations.

If user _agrees_ to apply this update, the wallet exits immediately with an exit code `20`. In this case, as described above, launcher restarts a node and update will be applied.

If user _doesn't agree_ to apply this update, the wallet continues work. But when it _will_ be restarted (sooner or later), that update _will_ be applied.

Important: updater runs synchronously, we're starting it and after that waiting for the finish. If updater finished its work [successfully](https://github.com/input-output-hk/cardano-sl/blob/03510d04d243d1cb9ecb2a2bd1e5392d1b64bd33/src/launcher/Main.hs#L194), `.tar`-archive will be deleted.
