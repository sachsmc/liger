# Reproducibility materials for "Cross Direct Effects in Settings with Two Mediators"
## Authors: Erin E Gabriel, Arvid Sjölander, Dean Follmann, Michael C Sachs
## Date: 2022-06-17

Contents: 

1. Covid-data-liger.R - Generate the COVID-19 example data. Produces dataoutcovidliger.csv
2. true-values-covid.R - Compute the estimated values in the covid data based on a large sample. 
3. point-estimates.R - Compute point estimates and bootstrap confidence intervals for the two examples. 
4. example-figures.R - Create the figures for the example. 
5. bounds-functions - Directory that contains the R functions to compute the bounds given conditional probabilities. 
6. simulation-study.R - Run the simulation study. This was run on a SLURM cluster with a command like: `sbatch --array=1-500 --wrap="Rscript simulation-study.R"`
7. analyze-simulations.R - Analyze the results from the simulation study and compile them into a table. 


# License

Copyright 2022 Michael C Sachs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
