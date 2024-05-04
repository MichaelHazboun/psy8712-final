# psy8712-final

This repository is an analysis of the 2018 General Social Survey (GSS) data set, which can be found [here](https://gss.norc.org/get-the-data/spss). The analyses are primarily focused on the relationship between family income and various other variables, such as age and number of pets owned. Additionally, the data set is used in an attempt to predict income through the utilization of machine learning on a large number of the variables in the data set. Various machine learning models were utilized and a comparison document, of the models, is provided within the larger repository. A[ shiny app](https://michaelhazboun.shinyapps.io/shiny_final/) is also created that can be utilized to learn more about our reduced model. A fully functional web-based binder, with the version of R and the packages utilized when the project was first made is also provided, and can be accessed [here](https://mybinder.org/v2/gh/MichaelHazboun/psy8712-final/HEAD?urlpath=rstudio).

## Project Reproduction:

The Work.R file, in the R folder, should be run first, followed by the app.R file, in the shiny/shiny_final folder. This way one can fully reproduce my project. However, if one desires to publish the shiny app they must first download and load the rsconnect package, complete a first time setup of their shinyapps.io account and the Rstudio they're using, followed by deploying the application by running the deployApp function with the appropriate path to the shiny_final directory from the working directory.

## Project Structure

### data

This folder contains the original data that's utilized in the R folder

### docs

This folder contains the various documentation that are associated with the project. For example, it contains the gss handbook, which can be read to gain a deeper understanding of the utilized data set, as well as the final.docx file, which contains APA-style write-up associate with this project.

### R

This folder contains Work.R which contains all the code that's needed to import the data from the data folder, analyse it and output the data for visual inspections, in the case of the out and figs folders, or for further use, in the case of the shiny/shiny_final folder.

### shiny

This folder contains the shiny_final folder which contains the app.R file and import.RDS file (which was an output from the Work.R file). This folder and everything in it are what's utilized to both run the shiny and deploy it.

### out

This folder contain all data output that have been exported from the project. Some examples include the Tukey analysis output as Tukey_thing.csv, or the descriptive statistics of some variables that were utilized in the project under descrip_stats.csv, or even the model_comp.csv file, which shows the R squares of the various models run across both two separate validation strategies.

### figs

This folder contains all visualization outputs from the project.

### Miscellaneous files in the main directory:

#### runtime.txt and install.R

This files are utilized in the creation of a functional web-based binder with the version of R and the packages utilized when the project was first made. The binder can be access [here](https://mybinder.org/v2/gh/MichaelHazboun/psy8712-final/HEAD?urlpath=rstudio).
