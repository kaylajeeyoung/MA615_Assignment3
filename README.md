# MA615_Assignment3
Assignment 3 - group project on hurricanes 
Contributors: Kayla Choi, Andrew Sisitzky, Lauren Temple

In this repository you will find: 
- Andrew_Data_Source.R
  - Maps of Hurricane Andrew and the underlying data repository
  - Pulls the data for the buoys
  - Organizes the data for further use
- Variogram Source.R 
  - Sources Andrew_Data_Source.R
  - Location data for the buoys
  - Manipulation of buoy and location data in preparation for variograms
  - Variograms on windspeed, windgust, and bariometric pressure
  - Basis for shiny map in the presentation
- Hurricane_Andrew_Slides.Rmd and corresponding html
  - Sources Variogram Source.R
  - Completed ioslides presentation
  - Shows an overview of Hurricane Andrew
  - The path of the hurricane
  - Interactive Shiny Map
    - select a date, then use mouse to draw a box around desired buoy(s) to see mean daily data
  - Variograms with interpretations
  - Interactive mouseover Map 
    - displays the maximum gust speed per buoy
  - Results Discussion