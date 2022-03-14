# projectiles-learning
# Prerequisites
* If you wish to run this in R studio, ensure it is downloaded.
* If you wish to run this in the terminal, ensure you have it installed. Instructions to do so can be found here: https://www.r-project.org/
# Output
1) First 10 lines of provided data.
2) MSE of X and Y on the test set after generating the model with the training set.
3) Comparisons of Errors of X and Y on High angle, Low angle, High power and Low Power shots.
4) Summary of Full models of X and Y incorporating all of the data.
5) A1 prediction of a projectile with the initial coordinates (0,0) and the next coordinates (0.707106781187, 0.658106781187). 
6) A graphic that displays the trajectory. (If running in terminal, it will be saved to the folder).
# How to Run
## RStudio
1) Open `Projectiles.R` file in RStudio.
2) Run using the `Run` button in top right.
## Terminal
1) Open terminal
2) Navigate to folder with `Projectiles.R` file.
3) Run the following command: `Rscript Projectiles.R`.
# Notes
* Graphics will not be displayed when running in the terminal. Instead, they will be generated and placed in the same folder under the name `Rplots.pdfs`