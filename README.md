---
title: "YGDP Dashboard README"
output: 
  html_document: 
    toc: yes
    keep_md: yes
    preserve_yaml: false
---



# YGDP Dashboard

README file for the YGDP Dashboard, developed in R Shiny.

### Files in this repo

This is a "tree" representation of the subfolders in this directory. The `.` at the top represents the top level directory (folder), called "ygdpDashboard". Note that this tree does not include all files in all the subdirectories.


```
## .
## ├── README.Rmd
## ├── README.html
## ├── README.md
## ├── README.pdf
## ├── dashboardFunctions.R
## ├── data
## │   ├── howTo
## │   ├── interpolations
## │   └── points
## ├── dataPrepScripts
## │   ├── INT.R
## │   └── PTS.R
## ├── howToAboutContent.R
## ├── rsconnect
## │   └── documents
## ├── shinydashboard.R
## ├── sketches
## │   ├── Dashboard sketch - Page 1.pdf
## │   ├── ygdpDashboardSketch.20201007.2.pdf
## │   └── ygdpDashboardSketch.20201007.pdf
## └── ygdpDashboard.Rproj
```

-   **README.Rmd:** used to generate README.pdf.

-   **README.pdf**: README document for this repo.

-   **dashboardFunctions.R**: supporting functions called by shinydashboard.R, used to create the app.

-   **data**/: directory containing the data that is used in the app.

    -   **howTo/**: labeled images, which are used in the "How to use" app section.

    -   **interpolations/**: data (hex rasters) used in the app's "Interpolation" mode.

    -   **points/**: data (sentence ratings and demographic characteristics) used in the app's "Points" mode.

-   **dataPrepScripts/**: scripts used to generate the data contained in "data/interpolations/" and "data/points"

    -   **INT.R**: script to generate hex rasters, contained in "data/interpolations/".

    -   **PTS.R**: script to pull data from the database for points mode and clean it up. Results are stored in "data/points/".

-   **howToAboutContent.R**: static content for the "How to use" and "About" sections of the app. This script is sourced in "shinydashboard.R". I've just chosen to write the content in a separate R script in order to reduce clutter in the main app script. This is the script to edit if you want to add or change the information presented in the "How to use" and "About" sections.

-   **sketches/**: preliminary dashboard sketches, which I used to design the final product.

-   **ygdpDashboard.Rproj**: the .Rproj "portal" for accessing the R scripts in this directory. Open this first, and then use the "files" pane in RStudio to open other scripts. If you don't do this, the file paths won't work and your app won't run! For more information about this, see the README in the top-level directory, "kaijaFiles".

### How to use this repo

1.  Make sure you have R and RStudio installed. For more information, see the README in "kaijaFiles".

2.  If you're reading this on GitHub and don't yet have this repository/folder downloaded, download it using the green "Code" button. Otherwise, proceed to step 3.

3.  Open the .Rproj file, "ygdpDashboard.Rproj".

4.  Use the "files" pane within RStudio to open "shinydashboard.R". Click "Run app" in the top right corner of the RStudio script pane.

#### Acknowledgements

App created by Kaija Gahm in 2020-21. Debugging help and other input from Ian Neidel. Additional help from Jake Riley, Asmae Toumi, and Jonathan Trattner via the [R4ds Slack channel](https://www.rfordatasci.com/), as well as \@nirgrahamuk and \@ismirsehregal via [RStudio Community](https://community.rstudio.com/).
