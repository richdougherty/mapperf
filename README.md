Experiment to improve performance when constructing immutable maps
in Scala.

* Use a mutable builder to construct `EmptyMap`, `Map1`-`Map4` or
  a `HashMap`. Avoid actually allocating the `Map` until `result`
  is called. The current builder simply creates a new immutable
  `Map` whenever a new object is added.
* Allow `HashMap` to mutate during the building phases. This avoids
  the need to create and throw away so many internal `HashTrieMap`
  nodes.
* Use the builder's `sizeHint` to guess the best size for
  `HashTrieMap` arrays. This avoids the need to create and throw
  away as many arrays when building. The size prediction is
  conservative. It will allocate too much space in fewer than
  1% of cases.

Comparison between "old" (Scala 2.12.4) and "new" experimental
implementation.

* `mapFactoryApplyArray` - calling `Map(...)` with an array of type `size`.
* `mapFactoryBuildWithSizeHint` - using `Map.newBuilder`, calling `sizeHint` and adding `size` elements
* `mapFactoryBuildWithoutSizeHint` - using `Map.newBuilder` and adding `size` elements
* `mapMap` - calling `map` on an `immutable.Map` with `size` elements

```
Benchmark                                       (factory)  (size)  Mode  Cnt       Score       Error  Units
MapBuilderBench.mapFactoryApplyArray                  old       0  avgt   24      50.543 ±     0.567  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       1  avgt   24      56.448 ±     0.669  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       2  avgt   24      69.322 ±     0.655  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       3  avgt   24     164.055 ±     1.377  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       4  avgt   24     208.204 ±     2.883  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       5  avgt   24     480.380 ±    32.844  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       8  avgt   24     663.635 ±    12.132  ns/op
MapBuilderBench.mapFactoryApplyArray                  old      32  avgt   24    2532.182 ±    53.170  ns/op
MapBuilderBench.mapFactoryApplyArray                  old     128  avgt   24   12337.833 ±   195.055  ns/op
MapBuilderBench.mapFactoryApplyArray                  old    4096  avgt   24  791122.669 ±  8462.053  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       0  avgt   24       5.451 ±     0.048  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       1  avgt   24      10.825 ±     0.301  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       2  avgt   24      18.638 ±     0.206  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       3  avgt   24      94.772 ±     3.449  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       4  avgt   24     109.942 ±     3.432  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       5  avgt   24     248.327 ±     8.555  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       8  avgt   24     374.252 ±    14.929  ns/op
MapBuilderBench.mapFactoryApplyArray                  new      32  avgt   24    1447.450 ±    31.964  ns/op
MapBuilderBench.mapFactoryApplyArray                  new     128  avgt   24    6591.271 ±    89.011  ns/op
MapBuilderBench.mapFactoryApplyArray                  new    4096  avgt   24  376147.637 ±  6772.861  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       0  avgt   24       3.788 ±     0.073  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       1  avgt   24      13.766 ±     0.235  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       2  avgt   24      32.400 ±     0.690  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       3  avgt   24     107.901 ±     1.089  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       4  avgt   24     150.448 ±     1.831  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       5  avgt   24     407.578 ±    28.652  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old       8  avgt   24     599.223 ±    14.876  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old      32  avgt   24    2361.342 ±    64.458  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old     128  avgt   24   11691.291 ±   150.465  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           old    4096  avgt   24  753399.774 ± 12817.605  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       0  avgt   24       8.295 ±     0.670  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       1  avgt   24      15.618 ±     0.200  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       2  avgt   24      23.134 ±     0.223  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       3  avgt   24      29.737 ±     0.283  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       4  avgt   24      37.816 ±     0.527  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       5  avgt   24     195.485 ±     8.354  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new       8  avgt   24     325.855 ±    11.024  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new      32  avgt   24    1399.208 ±    30.209  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new     128  avgt   24    6586.704 ±    76.782  ns/op
MapBuilderBench.mapFactoryBuildWithSizeHint           new    4096  avgt   24  357718.749 ±  6112.435  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       0  avgt   24       3.772 ±     0.069  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       1  avgt   24      13.698 ±     0.218  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       2  avgt   24      32.635 ±     0.386  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       3  avgt   24     106.909 ±     1.554  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       4  avgt   24     152.355 ±     5.393  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       5  avgt   24     386.090 ±     7.381  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       8  avgt   24     573.236 ±    13.440  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old      32  avgt   24    2306.904 ±    62.924  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old     128  avgt   24   11711.678 ±   111.226  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old    4096  avgt   24  749165.684 ±  8828.439  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       0  avgt   24       7.468 ±     0.074  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       1  avgt   24      16.116 ±     0.773  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       2  avgt   24      23.142 ±     0.279  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       3  avgt   24      29.906 ±     0.380  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       4  avgt   24      40.142 ±     0.693  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       5  avgt   24     208.731 ±     6.594  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       8  avgt   24     364.324 ±    10.304  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new      32  avgt   24    1489.138 ±    31.035  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new     128  avgt   24    6923.030 ±    75.405  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new    4096  avgt   24  381342.187 ±  5375.989  ns/op
MapBuilderBench.mapMap                                old       0  avgt   24      14.040 ±     0.137  ns/op
MapBuilderBench.mapMap                                old       1  avgt   24      21.457 ±     0.351  ns/op
MapBuilderBench.mapMap                                old       2  avgt   24      46.654 ±     1.077  ns/op
MapBuilderBench.mapMap                                old       3  avgt   24      51.547 ±     0.697  ns/op
MapBuilderBench.mapMap                                old       4  avgt   24      76.522 ±     3.333  ns/op
MapBuilderBench.mapMap                                old       5  avgt   24     387.851 ±    10.101  ns/op
MapBuilderBench.mapMap                                old       8  avgt   24     596.435 ±    14.275  ns/op
MapBuilderBench.mapMap                                old      32  avgt   24    2290.673 ±    99.637  ns/op
MapBuilderBench.mapMap                                old     128  avgt   24   11626.730 ±   146.220  ns/op
MapBuilderBench.mapMap                                old    4096  avgt   24  566320.013 ±  8984.228  ns/op
MapBuilderBench.mapMap                                new       0  avgt   24      12.819 ±     0.131  ns/op
MapBuilderBench.mapMap                                new       1  avgt   24      24.167 ±     0.258  ns/op
MapBuilderBench.mapMap                                new       2  avgt   24      32.339 ±     0.289  ns/op
MapBuilderBench.mapMap                                new       3  avgt   24      42.099 ±     0.445  ns/op
MapBuilderBench.mapMap                                new       4  avgt   24      51.737 ±     0.453  ns/op
MapBuilderBench.mapMap                                new       5  avgt   24     227.157 ±     6.518  ns/op
MapBuilderBench.mapMap                                new       8  avgt   24     371.187 ±     9.537  ns/op
MapBuilderBench.mapMap                                new      32  avgt   24    1516.283 ±    36.636  ns/op
MapBuilderBench.mapMap                                new     128  avgt   24    7233.829 ±   158.320  ns/op
MapBuilderBench.mapMap                                new    4096  avgt   24  307929.803 ±  7668.655  ns/op
```