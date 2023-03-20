
*Getting results
estimates use "N:\durable\projects\Perline\Eating disorders data\ED1_NRM"

*You can try opening the irt window in the GUI under statistics/IRT and use the report and graph options after you have loaded the results. There it is easy to play around with the graphs.

*Estimates
estat report
*How to get a more tidy sorted table (you can also change labels)
help estat report
estat report, byparm

*Graphs
*Item characteristic curves (not needed)
irtgraph icc, bcc
*Test information function ("reliability" over the score). Perhaps the one to plot.
irtgraph tif
*Test characteristic curve (latent variable (Theta) by score. Shows that we are getting a quite linear score above the mean (we do not have information below the mean). Possibly also this.
irtgraph tcc


*Do the same for ED8