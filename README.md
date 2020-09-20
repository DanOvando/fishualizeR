
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fishualizeR

<!-- badges: start -->

<!-- badges: end -->

fishulizeR is a shiny app, available at
<https://danovando.shinyapps.io/fishualizeR/>, built for helping users
walk through common visualization and quality control for the kinds of
data commonly encountered in fisheries. The current version of the app
only support length composition data (for example dockside measurements
of fish lengths or scuba surveys of fish lengths)

The app currently

1.  Allows users to upload length composition data stored in a csv file

<!-- end list -->

  - .csv is required to ensure that users to not attempt to store
    information in for example column colors
  - Length composition data can either be raw observations or binned
    counts (for example 10 individuals between 115 and 20 cm)

<!-- end list -->

2.  Allows users to plot and examine their raw data

<!-- end list -->

  - including color and faceting
  - Users can also correct common problems in consistent units
    (e.g. centimeters and milimeters) with length composition data

<!-- end list -->

3.  Allows users to assess their data coverage

<!-- end list -->

  - For example, see how many samples they have over time or by
    location, or both
  - Also allows them to see where data are missing
  - Data-coverage metrics can be downloaded for further examination

<!-- end list -->

4.  Allows users to aggregate their data

<!-- end list -->

  - Users can set the desired width of their aggregated length bins
  - They can then count the number of observations per length bin at any
    level of aggregation (for example year and fishing zone)
  - Aggregated data can then be downloaded and plotted
