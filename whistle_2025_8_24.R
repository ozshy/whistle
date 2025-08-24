# whistle_2025_mm_dd.R 
# Simulations (verification) of computations in a paper titled: "Whistleblowers and Product Safety."

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables

# Model parameters ####
#1 benchmark model
#2 delta2 > delta1 (all other parameters remain 1)
#3 rho3 > rho1 (all others remain 1) [section 7]
#4 beta4 > beta1 (all others remain 1) [section 7]

(v1 = 12) #basic valuation of product
#(v2 = 14) not used
(p = 8) #given market price
#
(cs = 4) # unit cost of safe product
(cr=1)# try [Yes, needed to have ns > 0]
#
(phi_s = 0.05) # failure probability of safer product
(phi_r = 0.15) # failure probability of riskier product
#
(delta1 = 50) #damage inflicted on buyers from failed product. Needs to be higher than delta_w computed below
(delta2 = 55)
#
(beta1 = 2)#burden parameter in WB utility function)
(beta4 = 3)
(betaw1 = ((v1+delta1)*(phi_r-phi_s) -cs)/2)# using delta1
(betaw2 = ((v1+delta2)*(phi_r-phi_s) -cs)/2)# using delta2
#
(rho1 = 40)# reputation parameter [section 7]
(rho3 = 50)

# check assumptions w.r.t the above parameters ####

# verify Assumption 3 
(delta_w = cs/(phi_r-phi_s))# eq (5)
(delta1 > delta_w)
(delta1 - delta_w)
(delta2 > delta_w)
(delta2 - delta_w)
#
(delta_max = v1*(1-phi_s)/phi_s -cs/phi_s)# eq (5)
(delta1 < delta_max)
(delta1 - delta_max)
(delta2 < delta_max)
(delta2 - delta_max)

# verify Assumption 5
beta1 > betaw1
beta1 - betaw1
beta4 > betaw1 # no change in delta here
beta4 - betaw1

# verify Assumption 6 (lower bound on rho in Section 7) 
rho1 > (cs-cr)/(phi_r-phi_s)
rho1 - (cs-cr)/(phi_r-phi_s)
rho3 > (cs-cr)/(phi_r-phi_s)
rho3 - (cs-cr)/(phi_r-phi_s)

# Section 3: The model ####
#
# eq (1) consumer utility
v1-p
-p-delta1

#
# eq (2) profit
(profits = p-cs)
(profitr = p-cr)

# eq (3) expected total surplus
(ets1 = (1-phi_s)*v1 -cs -phi_s*delta1)# using delta1
(etr1 = (1-phi_r)*v1 -cr -phi_r*delta1)
(ets2 = (1-phi_s)*v1 -cs -phi_s*delta2)# using delta2
(etr2 = (1-phi_r)*v1 -cr -phi_r*delta2)

# Section 4: Whistleblowers ####

# eq (4) expected total surplus with reproduction as safe
(etrsw1 = (1-phi_s)*v1 -cr -cs -phi_s*delta1)# using delta1
(etrsw2 = (1-phi_s)*v1 -cr -cs -phi_s*delta2)# using delta2

# re-verify Assumption 3 holds by showing that reproduction is beneficial
etrsw1 > etr1# using delta1
etrsw1 - etr1
etrsw2 > etr2# using delta2
etrsw2 - etr2

# Section 5: optimal compensation to WB ####
# verify Assumption 5
beta1 > betaw1# using delta1
beta1 - betaw1
beta4 > betaw2# using delta2
beta4 - betaw2

# eq (11) optimal compensation and WB probability
(mstar1 = ((v1+delta1)*(phi_r-phi_s)-cs)/2)# using delta1
(mstar2 = ((v1+delta2)*(phi_r-phi_s)-cs)/2)# using delta2

# verify m < beta
mstar1 < beta1# using delta1
mstar1 - beta1
mstar2 < beta4# using delta2
mstar2 - beta4

# bhat, also eq (11)
(bhatstar1 = ((v1+delta1)*(phi_r-phi_s)-cs)/(2*beta1))
bhatstar1 < 1 # verify bhat < 1
(bhatstar2 = ((v1+delta2)*(phi_r-phi_s)-cs)/(2*beta1))
bhatstar2 < 1 # verify bhat < 1

# Section 6: Deterrence ####
#
#eq (12) profits
(profits1 = p - cs)
(profitr1 = p - cr -bhatstar1*cs)# using delta1
(profits2 = p - cs)
(profitr2 = p - cr -bhatstar2*cs)# using delta2

# eq (13) conditions for Result 2
(bhatd1 = 1 -cr/cs)
bhatstar1 < bhatd1# 
(bhatd2 = 1 -cr/cs)
bhatstar2 < bhatd2# 
#
(md1 = beta1 * bhatd1)# above this compensation, cost-cutting is deterred
(md2 = beta1 * bhatd2)# above this compensation, cost-cutting is deterred

# eq (14): Condition on beta for Result 3
(betad1 = cs*((v1+delta1)*(phi_r-phi_s)-cs)/(2*(cs-cr)))# using delta1
beta1 > betad1# using delta1
beta1 - betad1
# this should imply
md1 > mstar1
md1 - mstar1
(betad2 = cs*((v1+delta2)*(phi_r-phi_s)-cs)/(2*(cs-cr)))# using delta2
beta4 > betad2# using delta2
beta4 - betad2
# this should imply
md2 > mstar2
md2 - mstar2

# Section 7: Reputation ####

# verify Assumption 6
rho1 > (cs-cr)/(phi_r-phi_s)
rho3 > (cs-cr)/(phi_r-phi_s)

#eq (16) fhat and upper bound on m
beta1*(cs-cr)/cs# => m below that yields 0< fhat < 1
beta4*(cs-cr)/cs# => m below that yields 0< fhat < 1

#
(m1.vec = seq(0, 2, 0.1))# not affected by delta
which(m1.vec == beta1*(cs-cr)/cs)
m1.vec[16]# for larger m fhat is out of bound

#eq (16) fhat as function of m
(fhat1.vec = (cs*(beta1-m1.vec) -cr*beta1)/(rho1*(beta1-m1.vec)*(phi_r-phi_s)))
fhat1.vec[which(m1.vec == beta1*(cs-cr)/cs)]# verify the value of m where fhat = 0
#
# verify calculation of fhat by using bhat (also in eq (16))
(bhat1.vec = m1.vec/beta1)
# then
(fhat1.vec = ((1-bhat1.vec)*cs -cr)/(rho1*(1-bhat1.vec)*(phi_r-phi_s)))
#
#
# below higher rho: rho3 > rho1
(fhat3.vec = (cs*(beta1-m1.vec) -cr*beta1)/(rho3*(beta1-m1.vec)*(phi_r-phi_s)))
fhat3.vec[which(m1.vec == beta1*(cs-cr)/cs)]# verify the value of m where fhat = 0
#
# below higher beta: beta4 > beta1
(fhat4.vec = (cs*(beta4-m1.vec) -cr*beta4)/(rho1*(beta4-m1.vec)*(phi_r-phi_s)))
fhat4.vec[which(m1.vec == beta4*(cs-cr)/cs)]# verify the value of m where fhat = 0

# Result 4: (fhat versus m ==> downward sloping)
(fhat.df = data.frame(m1.vec, "benchmark fhat"= fhat1.vec, "higher rho"= fhat3.vec, "higher beta"=fhat4.vec))


# Drawing Figure 4 in paper ####
#
# Refine and trim m1.vec
m1.vec
(m1_fig4.vec = seq(0, 1.7, 0.01))
#
#eq (16) fhat as function of m
(fhat1_fig4.vec = (cs*(beta1-m1_fig4.vec) -cr*beta1)/(rho1*(beta1-m1_fig4.vec)*(phi_r-phi_s)))
# 
#higher delta (no difference)
(fhat2_fig4.vec = (cs*(beta1-m1_fig4.vec) -cr*beta1)/(rho1*(beta1-m1_fig4.vec)*(phi_r-phi_s)))
#
# higher beta
(fhat4_fig4.vec = (cs*(beta4-m1_fig4.vec) -cr*beta4)/(rho1*(beta4-m1_fig4.vec)*(phi_r-phi_s)))

# eq (18), Figure 5 (in the paper)
(ebw1_fig4.vec = (m1_fig4.vec/beta1)*((v1+delta1 +(rho1*fhat1_fig4.vec)/2) *(phi_r-phi_s) -cs-m1_fig4.vec))
# m that max Ebw
(m1max_fig4 = m1_fig4.vec[which.max(ebw1_fig4.vec)])
(ebw1max = ebw1_fig4.vec[which.max(ebw1_fig4.vec)])
#
# higher delta (red)
(ebw2_fig4.vec = (m1_fig4.vec/beta1)*((v1+delta2 +(rho1*fhat2_fig4.vec)/2) *(phi_r-phi_s) -cs-m1_fig4.vec))
# m that max Ebw
(m2max_fig4 = m1_fig4.vec[which.max(ebw2_fig4.vec)])
(ebw2max = ebw2_fig4.vec[which.max(ebw2_fig4.vec)])
#
# higher beta (blue)
(ebw4_fig4.vec = (m1_fig4.vec/beta4)*((v1+delta1 +(rho1*fhat4_fig4.vec)/2) *(phi_r-phi_s) -cs-m1_fig4.vec))
# m that max Ebw
(m4max_fig4 = m1_fig4.vec[which.max(ebw4_fig4.vec)])
(ebw4max = ebw4_fig4.vec[which.max(ebw4_fig4.vec)])
#
# make it a data frame
(ebw_fig4.df = data.frame(m1_fig4.vec, ebw1_fig4.vec, ebw2_fig4.vec, ebw4_fig4.vec))

ggplot(ebw_fig4.df, aes(x=m1_fig4.vec)) +geom_line(aes(y=ebw1_fig4.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=ebw2_fig4.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=ebw4_fig4.vec), linetype="dotdash", linewidth=1.2, color="blue") + scale_x_continuous(breaks = seq(0,1.7,0.1)) + scale_y_continuous(breaks = round(seq(-0.7,1.4,0.1), 2)) +labs(x=TeX("Whistleblower compensation level: $m$"), y=TeX("Expected social benefit: $EB_W$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_hline(yintercept = 0, color = "black") +geom_segment(aes(x=m1max_fig4, y=0, xend = m1max_fig4, yend = ebw1max), linetype = "dotted", linewidth = 1.2) +geom_segment(aes(x=m2max_fig4, y=0, xend = m2max_fig4, yend = ebw2max), linetype = "dotted", size=1.2, color="red") +geom_segment(aes(x=m4max_fig4, y=0, xend = m4max_fig4 , yend = ebw4max), linetype = "dotted", linewidth=1.2, color="blue") +annotate("text", x = 0.60, y = 0.50, label = TeX("$\\beta\\uparrow$"), size = 8, color="blue") +annotate("text", x = 0.60, y = 1.1, label = TeX("$\\delta\\uparrow$"), size = 8, color="red")


# # Unused code ####
# # Below, I check that there are no typos in the above expression of ebw and retype eq (18) with bhat, and then substitute for m
# #
# (bhat1_verify = m1_fig4.vec/beta1)
# #
# (ebw1_verify.vec = bhat1_verify*((v1+delta1+rho1*fhat1_fig4.vec/2) *(phi_r-phi_s) -cs -m1_fig4.vec))
# # now verify that there are no typos
# ebw1_fig4.vec - ebw1_verify.vec
################