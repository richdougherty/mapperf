Experiment to improve performance when constructing immutable maps
in Scala. The main idea is to permit mutability during construction
time. A secondary idea is to try and predict the size of each node
of the trie to reduce the number resize operations and reallocations.

Comparison between "old" (Scala 2.12.4) and "new" experimental
implementation.

* `mapFactoryApplyArray` - calling `Map(...)` with an array of type `size`.
* `mapFactoryBuildWithoutSizeHint` - using `Map.newBuilder` and adding `size` elements
* `mapMap` - calling `map` on an `immutable.Map` with `size` elements

```
Benchmark                                       (factory)  (size)  Mode  Cnt       Score       Error  Units
MapBuilderBench.mapFactoryApplyArray                  old       0  avgt    8      50.717 ±     3.230  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       1  avgt    8      55.824 ±     1.263  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       2  avgt    8      68.766 ±     1.752  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       3  avgt    8     161.763 ±     4.004  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       4  avgt    8     208.936 ±     5.286  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       5  avgt    8     533.135 ±    16.513  ns/op
MapBuilderBench.mapFactoryApplyArray                  old       8  avgt    8     682.928 ±    50.435  ns/op
MapBuilderBench.mapFactoryApplyArray                  old      32  avgt    8    2558.214 ±   103.923  ns/op
MapBuilderBench.mapFactoryApplyArray                  old     128  avgt    8   12323.550 ±   237.471  ns/op
MapBuilderBench.mapFactoryApplyArray                  old    4096  avgt    8  780689.004 ± 11225.847  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       0  avgt    8       5.405 ±     0.144  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       1  avgt    8      11.033 ±     0.206  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       2  avgt    8      19.669 ±     1.341  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       3  avgt    8     118.978 ±     2.574  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       4  avgt    8     141.826 ±     3.365  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       5  avgt    8     253.324 ±    30.070  ns/op
MapBuilderBench.mapFactoryApplyArray                  new       8  avgt    8     308.349 ±    50.203  ns/op
MapBuilderBench.mapFactoryApplyArray                  new      32  avgt    8     684.762 ±    32.739  ns/op
MapBuilderBench.mapFactoryApplyArray                  new     128  avgt    8    2303.595 ±    45.406  ns/op
MapBuilderBench.mapFactoryApplyArray                  new    4096  avgt    8   88762.208 ±  5699.662  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       0  avgt    8       4.226 ±     0.120  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       1  avgt    8      15.505 ±     0.382  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       2  avgt    8      35.025 ±     0.471  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       3  avgt    8     110.407 ±     3.156  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       4  avgt    8     156.300 ±     4.883  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       5  avgt    8     391.497 ±    18.743  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old       8  avgt    8     607.021 ±    27.708  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old      32  avgt    8    2397.566 ±   124.978  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old     128  avgt    8   12102.749 ±   215.535  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        old    4096  avgt    8  792235.633 ± 24652.007  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       0  avgt    8       8.594 ±     0.255  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       1  avgt    8      25.819 ±     2.135  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       2  avgt    8      40.114 ±     1.183  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       3  avgt    8      57.716 ±     2.025  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       4  avgt    8      78.236 ±     1.592  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       5  avgt    8     223.052 ±    38.743  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new       8  avgt    8     261.890 ±    27.191  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new      32  avgt    8     644.801 ±    37.879  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new     128  avgt    8    2151.753 ±    82.023  ns/op
MapBuilderBench.mapFactoryBuildWithoutSizeHint        new    4096  avgt    8   86138.613 ±  4627.786  ns/op
MapBuilderBench.mapMap                                old       0  avgt    8      14.087 ±     0.228  ns/op
MapBuilderBench.mapMap                                old       1  avgt    8      21.085 ±     0.563  ns/op
MapBuilderBench.mapMap                                old       2  avgt    8      48.753 ±     0.411  ns/op
MapBuilderBench.mapMap                                old       3  avgt    8      56.404 ±     0.909  ns/op
MapBuilderBench.mapMap                                old       4  avgt    8      78.581 ±     2.046  ns/op
MapBuilderBench.mapMap                                old       5  avgt    8     405.640 ±     8.880  ns/op
MapBuilderBench.mapMap                                old       8  avgt    8     606.514 ±    30.221  ns/op
MapBuilderBench.mapMap                                old      32  avgt    8    2479.265 ±   166.306  ns/op
MapBuilderBench.mapMap                                old     128  avgt    8   12049.929 ±   151.396  ns/op
MapBuilderBench.mapMap                                old    4096  avgt    8  599005.702 ± 31986.851  ns/op
MapBuilderBench.mapMap                                new       0  avgt    8      13.444 ±     0.386  ns/op
MapBuilderBench.mapMap                                new       1  avgt    8      34.093 ±     1.022  ns/op
MapBuilderBench.mapMap                                new       2  avgt    8      50.772 ±     1.118  ns/op
MapBuilderBench.mapMap                                new       3  avgt    8      71.057 ±     1.541  ns/op
MapBuilderBench.mapMap                                new       4  avgt    8      93.131 ±     5.300  ns/op
MapBuilderBench.mapMap                                new       5  avgt    8     254.509 ±    49.934  ns/op
MapBuilderBench.mapMap                                new       8  avgt    8     280.262 ±     9.102  ns/op
MapBuilderBench.mapMap                                new      32  avgt    8     758.650 ±    54.933  ns/op
MapBuilderBench.mapMap                                new     128  avgt    8    2961.634 ±   115.233  ns/op
MapBuilderBench.mapMap                                new    4096  avgt    8  181680.879 ± 40384.365  ns/op
```