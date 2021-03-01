# sieve

![sieve](./doc/sieve.png)

**sieve** tests the endpoints it's been deployed on if they are leaky 💦, like a _sieve_. 


pros:
- it's written in **haskell**
- it's configurable via environment variables
- it works

cons:
- it might ruin your day

**sieve** is a essentialy a TCP/UDP blast, it ACKs out to a single host (a web server) trying to poke out of typical ports, right now these default to:

```haskell
ports :: [String]
["80", "443", "22", "21", "2222", "53"]
```

If you wish to test another set of ports, just set:

```sh
export PORTS = "80, 443"
```

You must set a host for **sieve** to know who to ACK to:

```haskell
endpoint :: String
```
```sh
export ENDPOINT = "127.0.0.1"
```

This must not contain any protocol information, just a DNS record, without the `.`

**sieve** requires two other important pieces of information to run correctly:

a **secret** to symmetrically encrypt the payload it's trying to blast out:

```haskell
secret :: String
```
```sh
export SECRET = "sup"
```

**sieve** runs under the assumption that it's being deployed on `x86_64`, but _should_ accomodate any architecture that **GHC** supports.

To test it locally for fun, you can run it via cabal:

```sh
cabal run sieve server
```

Will boot up the server locally, and by default listen on port `6666`

To blast it, and possibly other ports if you have your firewall configured correctly to forward TCP/UDP traffic to the port it's listening to:

```sh
# to blast ports 22 and 6666
PORTS="22, 6666" cabal run sieve blast udp
PORTS="22, 6666" cabal run sieve blast tcp
```

If you set this up in real life, the `iptables`, `nftables` or `pf` rules to forward all TCP/UDP traffic to a single port on an endpoint is a must. As you blast from the client to the remote host, the firewall will funnel all traffic into `sieve` to do its job, decrypt or drop a connection.

_make it rain_