# FoRmulaTV
FoRmulaTV is a simple web scraper for downloading spanish tv audience data from formulatv.com.

# How to install FoRmulaTV
In your R terminal type the following command:

```
devtools::install_github("meneos/FoRmulaTV")
```

# How to use FoRmulaTV

FoRmulaTV is a one function package. It downloads the audience data of the 25 most watched tv shows of spanish television of a given date interval. The following command will download audience data from the 5th until the 31th of january, 2018. FoRmulaTV will create a csv file in a specified path and will load it to the enviroment as a data frame.

```
library(FoRmulaTV)  # Load the package

formulatv("VALID-PATH-IN-YOUR-COMPUTER", "2018-01-05", "2018-01-31") # Run the function
```
