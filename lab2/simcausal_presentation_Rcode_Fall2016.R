# **********************************************************************
# R code in simcausal_presentation.pdf
# Prepared by: Oleg Sofrygin
# **********************************************************************

# Install from github (preferred):
devtools::install_github('osofr/simcausal', build_vignettes = FALSE)
# To install from CRAN:
# install.packages("simcausal")

# ----------------------------------------------------------------------
library(simcausal)
D <- DAG.empty()
D <- D +
  node("CVD", distr="rcat.b1", probs = c(0.5, 0.25, 0.25))
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Example for a single time point
# ----------------------------------------------------------------------
D <- D +
  node("A1C", distr="rnorm",
    mean = 5 + (CVD > 1)*10 + (CVD > 2)*5) +

  node("TI", distr="rbern",
	 prob = plogis(-0.5 - 0.3*CVD + 0.2*A1C)) +

  node("Y", distr="rbern",
	 prob = plogis(-3 + 1.2*TI + 0.1*CVD + 0.3*A1C))

setD <- set.DAG(D)
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
plotDAG(setD, vertex_attrs = list(size = 15, label.cex = 1.5))
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Longitudinal SEM example 1
# ----------------------------------------------------------------------
D <- DAG.empty()
D <- D +
  node("CVD", distr="rcat.b1", probs = c(0.5, 0.25, 0.25))

D <- D +
  node("A1C", t = 0, distr="rnorm",
	 mean = 5 + (CVD > 1)*10 + (CVD > 2)*5)

D <- D +
  node("A1C", t = 1:7, distr="rnorm",
	 mean = -TI[t-1]*10 + 5 + (CVD > 1)*10 + (CVD > 2)*5)

D <- D +
  node("TI", t = 0:7, distr="rbern",
	 prob = plogis(-5 - 0.3*CVD + 0.5*A1C[t] + 1.5*{if (t==0) {0} else {TI[t-1]}}))

D <- D +
  node("Y", t = 0:7, distr="rbern",
	 prob = plogis(-6 - 1.2*TI[t] + 0.1*CVD + {if (t==0) {0.3*A1C[t]} else {0.6*A1C[t]}}),
	 EFU = TRUE)

setDl <- set.DAG(D)

# ----------------------------------------------------------------------
plotDAG(setDl, tmax = 2, xjitter = 0.32, yjitter = 0.03,
  edge_attrs = list(width = 0.5, arrow.width = 0.4, arrow.size = 0.8),
  vertex_attrs = list(size = 19, label.cex = 1.5))
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Longitudinal SEM example 2
# Making TI[t] depend on a sum of previous TI for timepoits 0 to t-1
# ----------------------------------------------------------------------
D <- DAG.empty()
D <- D +
  node("CVD", distr="rcat.b1", probs = c(0.5, 0.25, 0.25))

D <- D +
  node("A1C", t = 0, distr="rnorm",
   mean = 5 + (CVD > 1)*10 + (CVD > 2)*5)

D <- D +
  node("A1C", t = 1:7, distr="rnorm",
   mean = -TI[t-1]*10 + 5 + (CVD > 1)*10 + (CVD > 2)*5)

D <- D +
  node("TI", t = 0, distr="rbern",
    prob = plogis(-5 - 0.3*CVD + 0.5*A1C[t]))

D <- D +
  node("TI", t = 1:7, distr="rbern",
    prob = plogis(-5 - 0.3*CVD + 0.5*A1C[t] + 1.5*sum(TI[0:(t-1)])))

setDl2 <- set.DAG(D)

# ----------------------------------------------------------------------
plotDAG(setDl2, tmax = 2, xjitter = 0.32, yjitter = 0.03,
  edge_attrs = list(width = 0.5, arrow.width = 0.4, arrow.size = 0.8),
  vertex_attrs = list(size = 19, label.cex = 1.5))
# ----------------------------------------------------------------------



# ----------------------------------------------------------------------
Odat_w <- sim(setDl, n = 5000, rndseed = 3)
head(Odat_w, 2)
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
Odat_l <- sim(setDl, n = 5000, wide = FALSE, rndseed = 3)
head(Odat_l)
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
newTRT0 <- node("TI",t=0, distr="rbern",
  prob = ifelse(A1C[0] >= theta,1,0))

newTRTp <- node("TI",t=1:7, distr="rbern",
  prob = ifelse(TI[t-1]==1,1,ifelse(A1C[t] >= theta,1,0)))

setDl <- setDl +
	action("early.switch", nodes = c(newTRT0, newTRTp), theta = 4) +
	action("late.switch", nodes = c(newTRT0, newTRTp), theta = 10)
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
lDAG <- set.targetE(setDl, outcome="Y", t=0:7, param="early.switch")
eval.target(lDAG, n = 5000)$res

lDAG <- set.targetE(setDl, outcome="Y", t=0:7, param="late.switch")
eval.target(lDAG, n = 5000)$res
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
setDl <- set.targetE(setDl, outcome="Y", t=0:7, param="early.switch");
surv_th1 <- 1 - eval.target(setDl, n = 5000)$res
setDl <- set.targetE(setDl, outcome="Y", t=0:7, param="late.switch");
surv_th2 <- 1 - eval.target(setDl, n = 5000)$res

plotSurvEst(surv = list(early.switch = surv_th1, late.switch = surv_th2),
			xindx = 1:8,
			ylab = "Counterfactual Survival, P(T>t)",
			ylim = c(0.4,1))
# ----------------------------------------------------------------------