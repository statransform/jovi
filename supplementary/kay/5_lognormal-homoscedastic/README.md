## Simple tests to evaluate ART on lognormal distributions with equal variances on the scale of the observed responses

For this simulations, we control the mean and standard deviation of the observed responses [as explained here](https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters):

The template file provides a dataset structure for a 4x3 within-subjects design with ``n=20``. 

The test files read the template and replace the response variable with observations drawn from lognormal distributions. They, then evaluate the Type I error rate of ART, PAR, RNK, and INT. 

``test-20-x1.R``: evaluates populations with a non-null effect on factor x1. Notice that the data simulation process is blind to the levels of x2.  
``test-20-x2.R``: evaluates populations with a non-null effect on factor x2. Notice that the data simulation process is blind to the levels of x1.  

The ``/log`` directory shows our outputs for 3000 iterations.

**Independent samples:** We provide additional scripts for 4x3 between-subjects designs (see files containing *-independent*). Those are much faster to run.