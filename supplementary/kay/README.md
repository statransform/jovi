## Additional code with simplified simulations 

This code is created in response to [Matthew Kay's post](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2628756195) to help him confirm our results, by following an alternative experimental approach.

Each folder contains simplified code for evaluating ART, along with results for PAR, RNK, and INT. We consider 4x3 within- and between-subjects designs, with factors x1, x2, and responses y.

As a first step, we created two data templates—one for ``n=20`` subjects and another for ``n=1000`` subjects. The larger sample size is used exclusively for discrete distributions to demonstrate the asymptotic breakdown of ART in discrete data as sample sizes increase.

We then use these templates to test the following scenarios:

1. *Binary* responses (0s and 1s) when all effects are null. 
2. *Binomial* responses when all effects are null.
3. *Ordinal* responses when all effects are null.
4. *Lognormal* responses, with a strong effect on a single factor (ether x1 or x2).
5. *Homoscedastic lognormal* responses, where we control the mean and standard deviation of the response variable (rather than the latent normal variable). Again, there is a strong effect on a single factor (ether x1 or x2).

The test scripts and their results for between-subjects analyses contain *-independent* in the file name. For these analyses, the subject column is ignored, and *aov* models are used. Simulation times for between-subjects designs are considerably faster than those for within-subjects designs.