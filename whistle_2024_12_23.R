# whistle_2024_x_y.R 
# Simulations (verification) of computations in a paper titled: "Whistleblowers and Product Safety."

# packages used
#library(ggplot2); theme_set(theme_bw())
#library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables

(v = 12) #basic valuation of product
(p = 8) #given market price
#
(cs = 4) # unit cost of safe product
(cr=1)# try [Yes, needed to have ns > 0]
#
(phi_s = 0.05) # failure probability of safer product
(phi_r = 0.15) # failure probability of riskier product
#
(delta = 30) #damage inflicted on buyers from failed product. Needs to be higher than delta_w computed below

#
(beta = 0.8)#prob that WB emerges (below, I compute beta_s, so I set it higher to provide incentive to produce the safer product)

# eq (2) profit
(profits = p-cs)
(profitr = p-cr)

# eq (3) expected total surplus
(ets = (1-phi_s)*v -cs -phi_s*delta)
(etr = (1-phi_r)*v -cr -phi_r*delta)

# eq (4) delta_s
(delta_s = (cs-cr)/(phi_r-phi_s) -v)

# eq (5) delta_max
(delta_max = (1-phi_s)*v/phi_s -cs/phi_s)

# eq (6) exp surplus with utilized WB info
(etrsw = (1-phi_s)*v -cr-cs -phi_s*delta)

# eq (7) delta_w
(etrsw >= etr)
(etrsw - etr)
(delta_w = cs/(phi_r-phi_s) -v)

# eq (8) social value of WB
(w = (v+delta)*(phi_r-phi_s) -cs)
etrsw -etr #verify

# eq (9) profit with WB
(profitrsw = p-cr-cs)
profitr #recall

# eq (10) beta_s
(beta_s = 1 - cr/cs)
profits #verify
(eprofitrsw = beta*profitrsw + (1-beta)*profitr)
profits >= eprofitrsw

# eq (11) expected utility with WB
(eus = (1-phi_s)*v -p -phi_s*delta)
(eur = (1-phi_r)*v -p -phi_r*delta)
#
(eurw = beta*eus + (1-beta)*eur)

# eq (13) exp total surplus with WB low beta => risky is sold unless WB emerges
(etrw = eprofitrsw + eurw)
# verify it the way it is typed in eq (13)
(1 -beta*phi_s -(1-beta)*phi_r)*v -(cr+beta*cs) - (beta*phi_s + (1-beta)*phi_r)*delta

# Section 5: adding compensation = n
# need to set n. First look at v+delta
(n = 25)
# I also need to reset beta to be beta < beta_s (as otherwise, compensation is not needed, see Figure 5)
beta_s# recall
#
(beta_n = 0.5)# this is just beta, reset for section 5 (compensation)

# eq (16) expected profit with penalty
(eprofitsn = p -cs -phi_s*n)
#
(eprofitrn = beta_n*(p-cr-cs-phi_s*n) + (1-beta_n)*(p-cr-phi_r*n))

# eq (17) ns as function of beta
(ns = ((1-beta_n)*cs-cr)/((1-beta_n)*(phi_r-phi_s)))

# eq (18) exp utility for low n => risky is produced unless WB emerges
# Below, exp utility from eq (11) is REDEFINED to include compensation
(eus = (1-phi_s)*v -p -phi_s*(n -delta))
(eur = (1-phi_r)*v -p -phi_r*(n -delta))
#
(eurn = beta*eus +(1-beta)*eur)

#eq (19)
(eusn = eus)
eusn > eurn# verify jump in exp utility in Figure 6


# graph from earlier versions, no longer applies
#ggplot(p.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=qaI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qbI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qaII.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(y=qbII.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.6,3.3,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium producer and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.225, y = 1.70, label =TeX("$p_B^I$"), size = 8, color="black") +annotate("text", x = 0.24, y = 2.08, label =TeX("$q_B^I$"), size = 8, color="blue") +annotate("text", x = 0.225, y = 1.90, label =TeX("$p_B^{II}$"), size = 8, color="red")  +annotate("text", x = 0.24, y = 2.39, label =TeX("$q_B^{II}$"), size = 8, color="magenta") +annotate("text", x = 0.20, y = 2.20, label =TeX("$p_A^I$"), size = 8, color="black") +annotate("text", x = 0.20, y = 2.54, label =TeX("$p_A^{II}$"), size = 8, color="red") +annotate("text", x = 0.15, y = 2.80, label =TeX("$q_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 3.13, label =TeX("$q_A^{II}$"), size = 8, color="magenta") +annotate("text", x = -0.003, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.67, label =TeX("A"), size = 8, color="black")  


################
################