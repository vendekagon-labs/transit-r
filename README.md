# transit (R)

Transit is a data format and a set of libraries for conveying values between
applications written in different languages. This package reads and writes
transit in R, in all three of its encodings: JSON, JSON-verbose and
MessagePack.

* [Rationale](http://blog.cognitect.com/blog/2014/7/22/transit)
* [Specification](http://github.com/cognitect/transit-format)

This implementation's major.minor version number corresponds to the version
of the Transit specification it supports.

Values roundtrip losslessly, including the transit types R has no direct
equivalent for, and output matches the reference implementation: every
transit-format msgpack exemplar is re-encoded byte for byte as transit-java
writes it, and cache codes match transit-clj's, including when the cache
fills up and starts over.

## Installation

The only dependency is [jsonlite](https://cran.r-project.org/package=jsonlite);
msgpack support is built in.

Install from GitHub with [remotes](https://cran.r-project.org/package=remotes)
or [pak](https://pak.r-lib.org):

```r
remotes::install_github("vendekagon-labs/transit-r")
# or
pak::pak("vendekagon-labs/transit-r")
```

To install over SSH, use
`remotes::install_git("git@github.com:vendekagon-labs/transit-r.git")`; add
`ref = "<tag or commit>"` (or `@<ref>` for `install_github()` and pak) to pin a
version. From a checkout: `install.packages("path/to/transit-r", repos = NULL, type = "source")`.

Requires R 4.1 or later (tested with R 4.6).

## Usage

```r
library(transit)

to_transit(list(1L, "two", transit_keyword("three")))
#> "[1,\"two\",\"~:three\"]"

to_transit(list(a = 1L), "json_verbose")
#> "{\"a\":1}"

bytes <- to_transit(list(1L, 2L), "msgpack")   # a raw vector

from_transit("[\"^ \",\"~:a\",[\"~#set\",[1,2]]]")
#> {:a #{1 2}}
```

`write_transit()` and `read_transit()` do the same with files and
connections. To process a sequence of values as they arrive, such as from a
pipe or socket, use `read_transit_stream()`:

```r
con <- file("stdin", "rb")
read_transit_stream(con, function(value) print(value), format = "msgpack")
```

Connections are read only as far as the end of each value, so reading never
waits for data beyond the value being read.

### Custom types

Pass `handlers` to write objects of your own classes, and to read your own
tags:

```r
point <- function(x, y) structure(list(x = x, y = y), class = "point")

to_transit(point(1L, 2L),
           handlers = list(point = function(p) transit_tagged("point", list(p$x, p$y))))
#> "[\"~#point\",[1,2]]"

from_transit("[\"~#point\",[1,2]]",
             handlers = list(point = function(rep) point(rep[[1]], rep[[2]])))
```

## Type mapping

| Transit type | Read as | Also written from |
|:-------------|:--------|:------------------|
| null | `NULL` | `NA` |
| string | character(1) | |
| boolean | logical(1) | |
| integer | integer, or `transit_int64()` outside 32 bits | |
| decimal (float) | double | |
| special numbers | `NaN`, `Inf`, `-Inf` | |
| arbitrary precision integer | `transit_bigint()` | |
| arbitrary precision decimal | `transit_decimal()` | |
| keyword | `transit_keyword()` | |
| symbol | `transit_symbol()` | |
| time | `POSIXct` (UTC) | `Date`, `POSIXlt` |
| uuid | `transit_uuid()` | |
| uri | `transit_uri()` | |
| bytes | raw vector | |
| array | unnamed list | atomic vectors of length other than one |
| map | `transit_map()` | named lists and named vectors (string keys) |
| set | `transit_set()` | |
| list | `transit_list()` | |
| link | `transit_link()` | |
| char, ratio and unknown tags | `transit_tagged()` | |

Arrays are read as lists, not vectors, so a one element array stays distinct
from a scalar. Maps are read as `transit_map()`s because transit map keys can
be any value (and keywords are distinct from strings); `m[[key]]` looks up a
key, and `as.list()` converts a map with string or keyword keys to a named
list. `transit_equal()` compares values, ignoring the order of maps and sets.

R strings can't contain the NUL character, so neither can strings read from
transit.

## Development

Run the tests with:

```sh
Rscript -e 'testthat::test_local()'   # or: R CMD build . && R CMD check transit_*.tar.gz
```

The exemplar tests read the example files from
[transit-format](http://github.com/cognitect/transit-format), which is
expected to be checked out next to this repo, or at `$TRANSIT_FORMAT_DIR`.

transit-format's verify harness drives `bin/roundtrip`, which loads the
package from source.

## Copyright and License

Copyright © 2026 Vendekagon Labs LLC

Based on the Java, Python and Julia implementations: Copyright © 2014
Cognitect, and Copyright © 2016 Russ Olsen, Ben Kamphaus.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.
