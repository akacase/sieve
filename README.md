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
export ENDPOINT = "capsulecorp.org"
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

And last but not least, a **detection** value; this is just a shared string so you can search through the web server logs to find.

```haskell
detection :: String
```
```sh
export DETECTION = "yo"
```

**sieve** runs under the assumption that it's being deployed on `x86_64`, but _should_ accomodate any architecture that **GHC** supports.

_make it rain_

