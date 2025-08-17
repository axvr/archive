# Halogen

> [!NOTE]
> An experiment to replace the [Halboy](https://github.com/jimmythompson/halboy) library (allowing interacting with APIs that expose HAL resources).  Ultimately, I would go on to supersede this project by embracing the data aspect of HAL resources by [creating a single, ~10 line function](../misc/hal.clj) that would resolve HAL links from a response and directly use standard Clojure HTTP client libraries.
>
> This doesn't mean this project was a total waste of time though as the `types.clj` file would later inspire future work of mine on providing consistent interfaces for encoding and decoding across HTTP clients and servers.

A compliant and reliable HAL library for Clojure.


## Installation

Add the following to your `deps.edn` file:

```clj
{:deps {uk.axvr/halogen
        {:git/tag "v1.0" :git/sha "xxxxxx"
         :git/url "https://github.com/axvr/halogen.git"}}}
```


## Usage

**Work in progress.**

```clj
(require '[uk.axvr.halogen :as hal])
```


## Resources

- [HAL RFC](https://github.com/mikekelly/hal-rfc) (expired)
- [HAL Specification](https://stateless.group/hal_specification.html)
- Web Linking: [RFC 5988](https://datatracker.ietf.org/doc/html/rfc5988)
- URI Template: [RFC 6570](https://datatracker.ietf.org/doc/html/rfc6570)
  - [uritemplate-clj](https://github.com/mwkuster/uritemplate-clj)
- The 'profile' Link Relation Type: [RFC 6909](https://www.rfc-editor.org/rfc/rfc6906.html)


## Legal

No rights reserved.

All source code, documentation and associated files packaged and distributed
with "uk.axvr.halogen" are dedicated to the public domain. A full copy of the
CC0 (Creative Commons Zero 1.0 Universal) public domain dedication can be found
in the `COPYING` file.

The author is not aware of any patent claims which may affect the use,
modification or distribution of this software.
