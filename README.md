# ygdpDashboard
Interactive dashboard for YGDP survey data.

The current version of the app can be found here: https://kaijagahm.shinyapps.io/ygdpDashboard/


**Known bugs**
*For a full list of known issues, please see the [issue tracker](https://github.com/kaijagahm/ygdpDashboard/issues).*

1. [URL bookmarking fails](https://github.com/kaijagahm/ygdpDashboard/issues/33) for a couple app features. 
  - It doesn't bookmark the state of the leaflet-native checkbox (the "include points that don't meet the criteria" checkbox at the top right corner of the map). 
  - It doesn't bookmark the display settings in interpolation mode (i.e. which coloration method you've chosen for the hexagon rasters)
  - It doesn't bookmark the map zoom and view. My guess is that this (and the leaflet checkbox) are due to URL bookmarking being unable to access internal Leaflet settings. For bookmarking to work on those settings, I would have to rewrite the code so that reactives/other values in the code save the settings and Leaflet listens to them, or something.
