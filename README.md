# hash-poc

Some example implementations and benchmarks for a fast, efficient file hashing
function that can normalize whitespace on the fly.

Run with `stack run -- --output bench.html` and open `bench.html` in your
browser to see some charts.

## approaches compared

(names below correspond to lines in the benchmarks)

* `rawLazy`: hashing a string in-memory in one loop using `Data.Text.Lazy`
* `lazy`: preparing and hashing a string in-memory in one loop using `Data.Text.Lazy`
* `strict`: preparing and hashing a string in-memory in one loop using `Data.Text`
* `lazyIO`: preparing and hashing a string read lazily from a file using `Data.Text.Lazy`
* `strictIO`: preparing and hashing a string read eagerly from a file using `Data.Text`

The `2n` and `2nIO` implementations use two loops; one for preprocessing and one
for hashing. They are provided for n=100 only as an illustration of how horribly
inefficient that approach is. Adding benchmarks for `2n` and `2nIO` with larger
values of n makes the benchmarks take way too long.

## conclusion

In general, performance differences between the lazy and strict forms are fairly
minor, but the strict forms clearly outperform the lazy ones in time. I suspect
that memory use is better with laziness, but these results do not provide any
data on memory at all.
