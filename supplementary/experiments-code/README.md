## Experimental code 

The folder includes our experimental code, written in R. Each script ``experiment\*.R`` corresponds to an individual experiment whose results are reported in the main article. The scripts ``appendix-experiment\*.R`` correspond instead to experiments whose results are reported in Appendix I. The files ``utils-\*.R`` provide the necessary functions for the simulations. Finally, the scripts ``testing-\*.R`` can be used to simply test the data generation and analysis code. 

You may need to install several R packages to make the scripts run: 

- ``install.packages('foreach')`` and  ``install.packages('parallel')``: parallel code execution
- ``install.packages('lmerTest')``: *lmer* models
- ``install.packages('ARTool')``: ARTool implementation
- ``install.packages('dplyr')`` and ``install.packages('tidyr')``: data manipulation libraries
- ``install.packages('POSSA')``: required by some scripts of the Appendix

By default, the scripts run on four parallel cores and perform 5000 iterations. Each script may take from several hours to several days to complete and will write the results into a file under */logs* (currently empty). The logs produced running these scripts can be found under the folder [../experiments-results](../experiments-results/).

The file ``np.anova.R`` is downloaded from http://www.uni-koeln.de/~luepsen/R/ and provides Lüpsen's implementation of  multifactorial generalizations of nonparametric tests.
