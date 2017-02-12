Haskell `distribution` package
==============================

`distribution` is a package for manipulating finite discrete probability distributions.
It supports transformations of distributions, measurements and efficient sampling.
Plotting distributions as bar charts is also possible using the related `distribution-plot` package.


Usage example
-------------

Let's fire up `ghci` and dive right into the code.

First things first, the one line of imports.

```haskell
> import Data.Distribution
```

Let's define the distribution of outcomes of a fair dice and
compute its mean and standard deviation.

```haskell
> let d6 = uniform [1 .. 6]
> mean d6
3.5
> standardDeviation d6
1.707825127659933
```

Let's now find 20 such dice and throw them all together !

```haskell
> let twentyD6 = times 20 d6
```

What's the probability that the sum of those dice is between `60` and `180` ?
Let's find out !

```
probability (\ x -> x >= 60 && x <= 180) twentyD6
1672525186848683 % 1828079220031488
```

The probabilities are computed exactly, and so whenever probabilities are queried,
a value of type `Rational` is returned. To make sense of this particular value, let's just
covert it to floating point:

```haskell
> fromRational it
0.9149084834626994
```

Let's now compute a much more fancier distribution.
For convenience, we will make use of the monadic interface of distributions.

```haskell
> let experiment = do { n <- from d6; m <- from $ uniform [ 1 .. n ]; return (n * m) }
> let distribution = run experiment
> probabilityAt 36 distribution
1 % 36
> probabilityAt 25 distribution 
1 % 30
```

Now, let's sample from that distribution. To do so, we must first build a `Generator`.

```haskell
> let generator = fromDistribution distribution
```

Once this generator is built, we can query it in constant time.

```haskell
> import Control.Monad.Random
> getSample generator 
25
> getSample generator 
36
> getSample generator 
3
```

Overview
--------

The `distribution` package contains several modules:

* `Data.Distribution.Core` which defines distributions and combinators over distributions,
* `Data.Distribution.Measure` which includes measures such as probability, expectation and variance,
* `Data.Distribution.Monadic` which includes proposes a monadic interface to manipulate distributions,
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

Copyright 2014-2017 Romain Edelmann

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
