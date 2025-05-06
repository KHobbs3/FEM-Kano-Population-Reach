# Population Coverage Analysis for Arewa and Freedom Broadcast Areas

This project estimates the **population coverage of two radio broadcast polygons** (`Arewa` and `Freedom`) within Nigerian state boundaries using a gridded population raster. It also estimates the number of **women of reproductive age (WRA) with radio access** in the covered areas.

------------------------------------------------------------------------

Last updated: May 2025

Script created for internal analysis at Family Empowerment Media.

For questions or modifications, contact: Kaitlyn Hobbs at [khobbs\@familyempowermentmedia.org](mailto:khobbs@familyempowermentmedia.org).

## Data Sources

-   100-m constrained wold population data: [WorldPop](https://hub.worldpop.org/geodata/summary?id=28031)
-   radio station polygons: radio specifications collected from radio stations; rasters created via CloudRF modelling. See: [Github Repo: Radio Reach](https://github.com/KHobbs3/radio-reach/tree/master/cloudrf) for template settings.
-   State boundaries: [Humanitarian Data Exchange](https://data.humdata.org/dataset/cod-ab-nga)

## Output Columns

Each output CSV contains:

-   source_file: Either arewa_polygon or freedom_polygon.
-   state_population: Total population in the polygon portion intersecting the state.
-   population_coverage: Proportion of state population reached by the broadcast.
-   state: State name (ADM1_EN).
-   wra: Estimated number of WRA with radio access in that state's covered area.
