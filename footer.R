# Dashboard footer
source("dashboardFunctions.R")

FOOTER <- tags$footer("Created by Kaija Gahm for the YGDP, October 2020. Code at https://github.com/kaijagahm/ygdpDashboard.", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:18px;
              color: #e6e6e6;
              padding: 2px;
              background-color: #46505a;
              z-index: 500;
              font-size:75%") # z index governs what goes in front/in back. The left sidebar is apparently 810. I don't know what the right sidebar is. 500 seems to get it behind both.