###############################################################################
# Calculate true value of time specific means for each DTR 
# u=exp[C^{(a1,a2)}beta]
###############################################################################
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

u.plusplus <- exp(C.plusplus %*% beta.vec)
u.plusminus <- exp(C.plusminus %*% beta.vec)
u.minusplus <- exp(C.minusplus %*% beta.vec)
u.minusminus <- exp(C.minusminus %*% beta.vec)

###############################################################################
# Calculate true value of quantities of interest
###############################################################################
eos.means.plusplus <- L.eos.means %*% exp(C.plusplus %*% beta.vec)
eos.means.plusminus <- L.eos.means %*% exp(C.plusminus %*% beta.vec)
eos.means.minusplus <- L.eos.means %*% exp(C.minusplus %*% beta.vec)
eos.means.minusminus <- L.eos.means %*% exp(C.minusminus %*% beta.vec)

AUC.plusplus <- L.AUC %*% exp(C.plusplus %*% beta.vec)
AUC.plusminus <- L.AUC %*% exp(C.plusminus %*% beta.vec)
AUC.minusplus <- L.AUC %*% exp(C.minusplus %*% beta.vec)
AUC.minusminus <- L.AUC %*% exp(C.minusminus %*% beta.vec)

###############################################################################
# Calculate true value of contrasts
###############################################################################
stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
stacked.C.minusplus.minusminus <- rbind(C.minusplus, C.minusminus)

# Differences in end-of-study means
diff.eos.means.plusplus.plusminus <- D.eos.means %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
diff.eos.means.plusplus.minusplus <- D.eos.means %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
diff.eos.means.plusplus.minusminus <- D.eos.means %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
diff.eos.means.plusminus.minusplus <- D.eos.means %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
diff.eos.means.plusminus.minusminus <- D.eos.means %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
diff.eos.means.minusplus.minusminus <- D.eos.means %*% exp(stacked.C.minusplus.minusminus %*% beta.vec)

# Differences in AUC
diff.AUC.plusplus.plusminus <- D.AUC %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
diff.AUC.plusplus.minusplus <- D.AUC %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
diff.AUC.plusplus.minusminus <- D.AUC %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
diff.AUC.plusminus.minusplus <- D.AUC %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
diff.AUC.plusminus.minusminus <- D.AUC %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
diff.AUC.minusplus.minusminus <- D.AUC %*% exp(stacked.C.minusplus.minusminus %*% beta.vec)

