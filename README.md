
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapedit.addin

[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)

The goal of mapedit.addin is to simplify the creation of spatial data.

## Installation

You can install the released version of mapedit.addin from
[Github](https://github.com/mrjoh3/mapedit.addin) with:

``` r
devtools::install_github("mrjoh3/mapedit.addin")
```

## Usage

This simple addin allows you to interactively create a geometry object
that can then be saved and used in your code. To use just open the addin
add some geometry to the map and click on `Done` when completed.
Currently this will save `sf` object called `geom` into the global
environment and save a `geojson` called `saved_geometry.geojson` to the
current working directory.

## Future

Future updates will give the user more control over:

1.  the starting point of the map
2.  the name of output object
3.  filename