## Experimental results

This folder presents the raw files generated from our experimental scripts and used for the analysis of our JoVI article and appendix.

The log files generally contain the following columns: 
- The sample size *n*.
- The experimental *design*, such as *4x3* and *2x3*.
- The data distribution (*family*), such as *norm*, *lnorm*, and *binom*.
- The compared *method*, such as *PAR*, *RNK*, *INT*, and *ART*. 
- The *alpha* level, which is either *0.05* or *0.01*.
- The magnitude of main effects (e.g., *effectX1* and *effectX2*) and interaction effects (e.g., *effectX1X2*)
- The observed rate of positives for main effects (e.g., *rate1* and *rateX2*) and interacton effects (e.g., *rateX1X2*). Depending on the applied effects, this rate can represent the Type I error rate or the Power of the method.

Some files contain additional columns, such as the the max ratio of standard deviations among levels (*sd_ratio*) for experiments comparing the methods under unequal variances. 
