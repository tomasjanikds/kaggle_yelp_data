# O2 Assignment

This repository contains the necessary files and code for a most influential customers machine learning solution. The directory structure is organized as follows:

- `man`: This directory contains HTML report `user_reviews.html` with the solution along with a file generating this report `user_reviews.Rmd` should you wish to replicate it.

- `data/processed`: This directory contains all the data used for training, serving the machine learning model and final scoring dataset.

- `data/raw`: This directory contains the raw data files.

- `kaggle_yelp_data.Rproj`: This is the R project file associated with this repo.

- `renv.lock`: This file specifies the package dependencies and their versions for reproducibility using the R package `renv`.

- `src/R`: This directory contains the R scripts for the machine learning solution. The scripts are organized into the following workflow:

  1. `user_reviews.R`: This script runs the analysis. You can go throgh the script by sections.

  2. `functions.R`: This is a helper script with functions used in the solution.

  3. `kaggle_data.R`: This script contains instructions on how to download and read-in the raw data from kaggle.
 
  4. `shiny_data_prep.R`: This script runs data prep steps for Shiny application.

## Environment Setup

### RStudio

To replicate the environment in RStudio, follow these steps:

1. Clone the repository to your local machine:
   ```shell
   git clone https://github.com/tomasjanikds/kaggle_yelp_data.git
   ```

2. Make sure you have R and RStudio installed on your system. You can download and install R from the official R project website: [https://www.r-project.org/](https://www.r-project.org/), and RStudio from: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/).

3. Launch RStudio on your computer.

4. In RStudio, go to `File` > `Open Project` and navigate to the cloned repository's directory. Select the project file with the `.Rproj` extension and click `Open`.

5. RStudio will automatically detect the `renv.lock` file in the project directory and prompt you to restore the project environment. Click on the `Restore` button to initiate the process.

6. RStudio will install the required R packages and their specific versions as specified in the `renv.lock` file. This may take some time depending on the size of the environment.

7. Once the environment is restored, you can navigate to the `src/R` directory and run the machine learning solution by executing the R scripts in the desired order. Refer to the repository's documentation or the `README.md` file for instructions on how to execute the solution and its various scripts.

### Other

If you are not using RStudio:

1. Clone the repository to your local machine:
   ```shell
   git clone https://github.com/tomasjanikds/kaggle_yelp_data.git
   ```
   
2. Open a terminal or command prompt and navigate to the cloned repository's directory:
  ```shell
  cd kaggle_yelp_data
  ```

3. Install the `renv` package, which will help you recreate the project's environment:
  ```shell
  R -e "install.packages('renv')"
  ```

4. Restore the project's R environment using `renv` and the `renv.lock` file provided in the repository:
  ```shell
  R -e "renv::restore()"
  ```

5. Once the environment is restored, you can navigate to the `src/R` directory and run the machine learning solution by executing the R scripts in the desired order. Refer to the repository's documentation or the `README.md` file for instructions on how to execute the solution and its various scripts.

By following these steps, you should be able to replicate the environment for the machine learning solution, regardless of whether you are using RStudio or not.

## Shiny application

For the geo-spatial analysis, please see my app here: [Late Coffee](http://tjanik.shinyapps.io/late_coffee/)
