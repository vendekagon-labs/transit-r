## Contributing

transit-r is maintained by [Vendekagon Labs](https://github.com/vendekagon-labs).
Report problems and suggest changes with GitHub
[issues](https://github.com/vendekagon-labs/transit-r/issues).

Pull requests are welcome, but make sure they're well tested first: add tests
for what you change, and check that the tests
(`Rscript -e 'testthat::test_local()'`), `R CMD check` and transit-format's
verify harness (`bin/verify`, which needs the
[clojure CLI](https://clojure.org/guides/install_clojure)) pass, with both the
compiled code and the R implementation (`options(transit.native = FALSE)`).
Changes to the C code in `src/` go to
[transit-c](https://github.com/vendekagon-labs/transit-c) first.
