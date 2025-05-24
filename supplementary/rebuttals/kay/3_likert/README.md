## Simple tests to evaluate ART on ordinal data

The template files provide a structure for the sampled datasets. We provide templates for a 4x3 within-subjects design and two sample sizes: ``n=20`` and ``n=1000``.

The test files read the corresponding templates and replace the response variable with observations drawn from an ordinal scale. They then evaluate the Type I error rate of ART, PAR, RNK, and INT. 

The ``/log`` directory shows our outputs for ordinal distributions (responses to Likert items) with 5 levels with flexible thresholds. For *n=20*, we used 3000 iterations but only 100 iterations for *n=1000*, because ART's Type I error rates are clearly huge in this case. 

**Independent samples:** We provide additional scripts for 4x3 between-subjects designs (see files containing *-independent*). Those are much faster to run.