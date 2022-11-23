# gliderBallastingApp


UNDER DEVELOPMENT

This Shiny app is intended to be used by those who ballast gliders prior to missions that are
deployed and recovered outside of the Halifax Harbour. The app allows the user to choose a range 
of dates the glider is intended on being deployed and recovered. Daily climatology profiles of 
temperature, salinity, and density at the nearby fixed-station, HL_02, are shown for these date 
ranges, and average near surface values are returned. The user can also display the most recent 
profile taken at HL_02, and any profiles taken in the defined deployment and recovery date ranges 
from previous years.

# How to use:


1. Install R and RStudio. If using a DFO computer, it is best to install through the software center.

2. Download the Shiny app code from this repository, through either
   direct download, cloning it using the Github Desktop app, or by typing:
   ```
   $ git clone https://github.com/DFOglider/gliderBallastingApp.git
   ```
   in a terminal.

   Cloning is recommended, as updating the app can be done
   with a simple `git pull` command in a terminal or by pulling changes in the Github Desktop app.

3. You will need to install several packages (as listed at the
   top of `app.R` and `01_readSourceAndArchive1999toPresent.R`). 
   To do this run the following in the R console (copy/paste):
   ```r
   install.packages(c('shiny', 'oce', 'ocedata', 'sp'))
   ```
   Install the `devtools` package, which can be done via `install.packages('devtools')`.   
   
   One non-CRAN package will also have to be installed from gitHub by running the following lines
   in the R console (copy/paste):
   ```r
   library(devtools)
   install_github('clayton33/csasAtlPhys', ref = 'master')
   ```

4. After installing all the required packages, the next step is to read in HL_02 data. The user will have 
   to be connected to the Network to be able to access the data. Note that it is
   best to run this script while in the office. Over VPN it could take some time (I'd recommend starting it
   at the end of a workday, and let it run in the evening). Open RStudio through the `.Rproj` file (this will open 
   RStudio in the appropriate working directory). Then, open `01_readSourceAndArchive1999toPresent.R`. 
   If it is the users first time reading it data, ensure that
  `reReadData` on `line 6` is set to `TRUE`. Run `01_readSourceAndArchive1999toPresent.R`.
   
5. Once all of the archive data is read in once, `reReadData` 
   can be set to `FALSE` for future use. When set to `FALSE` it will only read in 
   newly added data to the archive and will always read in any data that is not in the archive and is 
   still in the source directory. 
   
6. Once all HL_02 data is read, open `app.R` in RStudio, and run the app by clicking "Run App" at the top of the code editor.

7. Once the app is running, choose the dates for deployment and recovery, and apply the options that are of interest. The app will always initialize the deployment date near the current date and the recovery date three weeks after deployment.
 