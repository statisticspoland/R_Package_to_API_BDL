# bdl.maps

This package contains set of 6 spatial maps for each NUTS level of Poland.
Each map has unit's `name`, unit's NUTS 12 character `id` and `geometry` with
spatial data columns.

Additionaly there is included `generate_map()` function that can be used to
fill maps with [BDL](https://bdl.stat.gov.pl/BDL) API data using `bdl` package.

This package provides the following data tables.

* `level1_map` with level 1 units of Poland
* `level2_map` with level 2 units of Poland
* `level3_map` with level 3 units of Poland
* `level4_map` with level 4 units of Poland
* `level5_map` with level 5 units of Poland
* `level6_map` with level 6 units of Poland

More info about Poland's NUTS codes and other metadata can be found [here](https://bdl.stat.gov.pl/BDL/metadane).
