# Dirigo

_2022-02-05 – 2023-04-17_

> [I direct to a place, guide, steer](https://en.wiktionary.org/wiki/dirigo#Latin)

The micro web-forwarding and link shortener service that I used to use for
`axvr.uk` until I got tired of self-hosting it.

This version was in the middle of a migration away from Trafik to bring ACME
certifcate refreshing into this Clojure service.  I unfortunately never
finished that move so this latest version remains broken.  Check back through
the Git history if you want to find a working version.

Maybe Dirigo will return some day if I decide to self-host again.


## Connect domain name

### Domain root

Create an A record pointing to `18.132.100.149`.

### Subdomain

Create a CNAME record pointing to `axvr.uk.`.


## Running

```sh
clojure -X:run
```


## Legal

_Public domain.  No rights reserved._
