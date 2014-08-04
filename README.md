Haskell `distribution` package
==============================

`distribution` is a package for manipulating finite discrete probability distributions.
It supports transformations of distributions, measurements and efficient sampling.
Plotting distributions as bar charts is also possible using the related `distribution-plot` package.

Overview
--------

The `distribution` package contains several modules:

* `Data.Distribution.Core` which defines distributions and combinators over distributions,
* `Data.Distribution.Measure` which includes measures such as probability, expectation and variance,
* `Data.Distribution.Sample` which implements Walker's [Alias method](http://en.wikipedia.org/wiki/Alias_method) for efficiently sampling random values from distributions.

In addition, some domain specific distributions and functions are proposed by default:

* `Data.Distribution.Domain.Dice` defines dice and functions on dice.
* `Data.Distribution.Domain.Coin` defines the same for coins.
* `Data.Distribution.Aggregator` contains functions for transforming distributions to cumulative distributions and other useful methods.

Each module and each function is extensively documented.

Related packages
----------------

See the `distribution-plot` package for plotting distributions to files. This package is also [available on GitHub](https://github.com/redelmann/haskell-distribution-plot).

License
-------

Copyright 2014 Romain Edelmann

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
