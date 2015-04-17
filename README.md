Look! It's your data!
======

## A shiny app for data visualization

This is a [Shiny](http://shiny.rstudio.com/) app that generates graphics used to explore your data set. 

Written in [R](http://cran.r-project.org/), this shiny app requires the following packages:

  * shiny
  * foreign (to load SAS xport files)
  * haven (to load SAS binary files .sas7bdat)
  * ggplot2 
  * reshape2
  * RColorBrewer
  * markdown (maybe?)

## Description
If a picture is worth a thousand words, then how many tables are a single visualization worth? [Exploratory data analysis](http://en.wikipedia.org/wiki/Exploratory_data_analysis) is a great way to see what is and is not in your dataset.
The xportEDA app makes it easy to visualize your data quickly, without requiring programming effort to get a jump on your data wrangling.

You supply the app with a data file. The app can read in a data.frame from a SAS xpt, SAS sas7bdat, csv or rdata file, and generates a set of data visualizations.

The app first classifies the variables as continuous, logical or categorical. Any variable with only 2 unique values is interpreted as logical. An ad-hoc definition for categorical variables is any factor plus any variable with more than 2 and less than 10 unique values. By default, character variables are converted to factors, however if we have more than 20 levels, we will not show a panel figure for that variable.

Since we often are working in time-to-event settings, the app searches the variable names for some of our “standard” time related variable names to use for the x-axis. Typically, we use a “date of procedure” for this. However, if your data does not have a “time” variable name, we will select the first continuous variable for the x-axis. This variable can be changed though the Shiny interface. 

The app creates a faceted set of histograms for all categorical and logical variables, and another set of scatter plots for all continuous variables. For continuous variables, we also indicate missing values with rug marks at the bottom of each panel. 

A separate page is set up for visualizing individual variables, making it easy to export a single figure for use in reports or other communications. Useful for when your collaborators do not believe you are missing large chunks of data in a variable, or there are negative values for strictly positive variables, like height.

We also include a data summary page for further data debugging purposes.

## Running the App
To try it out, you can see it on the shinapps.io site
https://ehrlinger.shinyapps.io/xportEDA/

I have also posted the app code to a GitHub repository where you can download it, and try it out. Let me know how it goes, report bugs or contribute back. I’d love to make this better, and learn more Shiny tricks along the way.

The easiest way is to run the app locally is to download it from the https://github.com/ehrlinger/xportEDA repository.

Then run it from R
```
R> library(shiny)
R> runApp()
```

or run it in R directly from the repository:

```
R> shiny::runGitHub("ehrlinger/xportEDA")
```

## Issues and Caveats

I could put the standard “use at your own risk” disclaimer here. I will also add:

The app is written with my specific problem domain in mind though I am open for suggestions on how to improve it.

We tend to use time to event data (working in a hospital after all).

Our group uses SAS predominantly, hence the “xport” functionality and the app naming structure.

xportEDA will have trouble with large p data sets, as I have not figured out how to make shiny extend the figures indefinitely down the page. I do dynamically set the number of columns in an effort to control how small the panel plots get. But if you get into the 75 categorical or continuous variable range, it may become illegible.

Progress not perfection! The best way to start, is to start.

## Blog article posted at:
http://jehrlinger.wordpress.com/2014/12/10/look-its-your-data/
