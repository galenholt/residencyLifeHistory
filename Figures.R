# Figures at the time of paper submission

# libraries
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(magrittr)
library(viridis)
library(RColorBrewer)
library(directlabels)

# On to code --------------------------------------------------------------


# Figure control ----------------------------------------------------------

# Because some of the polygons are a pain and don't resolve over several tries
figfinder <- function(ggName, ntries = 1000) {
    bo=0
    while(bo!=ntries){
        x = try(ggName,silent=TRUE)
        if (class(x)=="try-error") {
            cat("ERROR1: ", x, "\n")
            Sys.sleep(0.1)
            bo <- bo+1
            print(bo)
        } else
            break 
    }
    ggName
}

# Multipanel setup
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# Themes for various presentaiton formats
texttheme <- theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   legend.text=element_text(size=8), 
                   legend.title = element_text(size = 8))

talktheme <- theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=16),
                   strip.text=element_text(size=16),
                   plot.title=element_text(size=16))

posttheme <- theme_bw() + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=20),
          strip.text=element_text(size=20),
          plot.title=element_text(size=20))

pubtheme<-theme_bw()+theme(strip.background=element_blank(),
                           plot.background=element_blank(),
                           axis.text=element_text(size=8),
                           axis.title=element_text(size=10),
                           strip.text=element_text(size=8),
                           legend.text = element_text(size = 8),
                           legend.title = element_text(size = 8),
                           plot.title=element_text(size=8),
                           panel.grid.major=element_blank(), 
                           panel.grid.minor=element_blank())

# Color ramp
newpal <- colorRampPalette(c('dodgerblue', 'darkorange'), space = "Lab") # firebrick4


# Create the dataframes used to illustrate --------------------------------

# These just make gridded values of variables given the others
# It's almost possible to do this all with one df, but because of what gets set
# vs. calculated, have to make separately
# Need to do three things:
# 1. Calculate R for a given survival ratio and larval amphidromy
# 2. calculate larval amphidromy for a given R and survival ratio
# 3. Calculate survival ratio for a given R and larval amphidromy
# 4. calculate the odds (really, this is the same as calculating alpha)

# First, set the set values

setR <- round(seq(from = 0, to = 1, by = 0.01),2) # resident fraction
logS <- round(seq(from = -5, to = 5, by = 0.01),2) # log of survival fraction (using S because F means false)
setS <- 10^logS # the survival fraction itself (using S because F means false)
seta <- round(seq(from = 0, to = 1, by = 0.01),2) # Set the larval amphidromy rate
logAO <- round(seq(from = -5, to = 5, by = 0.01),2) # log amphidromy odds
setAO <- 10^logAO # amphidromy odds
logRO <- round(seq(from = -10, to = 10, by = 0.01),2)
setRO <- c(10^logRO, 1.5, 4)

# Now, make the necessary data frames
calcR <- expand.grid(S = setS, alpha = seta)
calcR <- mutate(calcR,
                oddsA = alpha/(1-alpha),
                R = (S*(1-alpha)) / ((S*(1-alpha)) + alpha),
                Ro = S/(S+oddsA),
                oddsR = R/(1-R),
                roundOdds = round(oddsA, 2))

# Could make from above, but cleaner to just do the logged version separately
calcRO <- expand.grid(S = setS, oddsA = setAO)
calcRO <- mutate(calcRO,
                 alpha = oddsA/(1+oddsA),
                 R = S / (S+oddsA),
                 oddsR = R/(1-R),
                 roundOdds = round(oddsA, 2))

# Instead of looking at the values themselves, look at the changes (deltas)
# Let's say Fm = 1, alphm = 0.5 as centered references
deltF <- round(seq(from = -3, to = 3, by = 0.05),2)
# deltA <- round(seq(from = -0.5, to = 0.5, by = 0.01),2)
Fm = c(0.1, 0.5, 1, 2, 10)
alpham = c(0.5, 0.6, 0.8, 0.9)
alphan = round(seq(from = 0.01, to = 0.99, by = 0.01),2)

deltRa <- expand.grid(Fm, deltF, alpham, alphan)
names(deltRa) <- c('Fm', 'deltF', 'alpham', 'alphan')
deltR <- mutate(deltRa, Fn = Fm*(10^deltF), deltA = alpham - alphan,
                oddsn = alphan/(1-alphan), oddsm = alpham/(1-alpham),
                Rm = Fm/(Fm + oddsm), Rn = Fn/(Fn + oddsn),
                Rdiff = Rn-Rm, alphdiff = alpham-alphan, Frat = Fn/Fm)

# Log scale
# over ranges of S and alpha odds
logratS <- round(seq(from = -3, to = 3, by = 0.05),2) # log of survival fraction (using S because F means false)
logratA <- round(seq(from = -3, to = 3, by = 0.05),2) # log of alpha odds

SrAr <- expand.grid(logratS, logratA)
names(SrAr) <- c('logratS', 'logratA')
SrAr <- mutate(SrAr, logRrat = logratS + logratA, Srat = 10^logratS, Aorat = 10^logratA, Rrat = Srat*Aorat)


# Figure 2 ----------------------------------------------------------------

# Make the first panel- residents on y, survival on x, colors = amphidromy
# Residents on y, S on x, colors = amphidromy
mypal2 = newpal(9)
mypal2[5] = 'black'

alphcut <- round(seq(from = 0.1, to = 0.9, by = 0.1),2)
RvScA <- ggplot(subset(calcR, alpha %in% alphcut),
                aes(x = log(S,10), y = R, 
                    color = factor(alpha))) + 
    geom_line()
RvScA
RvScA <- RvScA + scale_color_discrete(type=mypal2, breaks = c(0.1, 0.5, 0.9)) +
    xlab(expression(log(italic(s[f])/italic(s[o])))) +
    ylab('Proportion Resident Adults') +
    labs(color='Larval\namphidromy') +
    guides(color=guide_legend(ncol=1)) +
    geom_line(y = 0.6, color = 'grey60', linetype = 'dashed')+
    geom_line(y = 0.8, color = 'grey60', linetype = 'dashed')
RvScA

# Make the second panel- Change in residency as heatmap against change in
# survival ratio and amphidromy
    # Because arithmetic scale, the baseline shifts where the heat values are
    # relative to the axes. For clarity, center it with everything even: alpha =
    # 0.5, sh/sa = 1
RD <- ggplot(subset(deltR, Fm %in% c(1) & alpham %in% c(0.5)), 
             aes(x = alphdiff, y = deltF, z = Rdiff)) +
    geom_raster(aes(fill=Rdiff), show.legend = TRUE) +
    scale_fill_viridis(option = 'viridis')+
    geom_contour(aes(colour = ..level..), binwidth = 0.2) +
    scale_color_gradient(limits=range(deltR$Rdiff), high = 'white', low = 'white', guide = 'none') + 
    ylab(expression(paste("log", bgroup("(",
                                        frac((italic(s[f]) / italic(s[o]))[italic(new)], (italic(s[f]) / italic(s[o]))[italic(initial)]),
                                        ")")))) +
    xlab(expression(alpha[italic(initial)] - alpha[italic(new)])) +
    labs(fill = expression(R[italic(new)] - R[italic(initial)])) +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    geom_dl(aes(label=..level..), binwidth = 0.2, color = 'white',
            method=list("far.from.others.borders"), stat="contour")

RD

#### SAVE FIGURE 2 TEXT  ###
pdf("Fig2.pdf", width=12/2.54, height=12/2.54, useDingbats = FALSE)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
print(figfinder(RvScA + pubtheme +
                    ylab(expression(atop('Proportion Resident Adults', italic(R)))) +
                    xlab(expression(paste('Log Survival Ratio ', (log(italic(s[f])/italic(s[o])))))) +
                    labs(color = expression(atop('Larval\namphidromy', alpha))) +
                    # annotate("text", x = -4, y = 0.95, label = "a") +
                    # geom_text(label = 'a', x = -7.1, y = 0.99, color = 'black') +
                    guides(color=guide_legend(ncol = 1)) +
                    theme(legend.position = 'right')), #+
      #coord_cartesian(clip = 'off')),
      vp = vplayout(1, 1))
print(RD + pubtheme + theme(panel.grid = element_blank()) +
          ylab(expression(atop('Change in survival ratio', 
                               paste("log", bgroup("(",
                                                   frac((italic(s[f]) / italic(s[o]))[italic(new)], (italic(s[f]) / italic(s[o]))[italic(initial)]),
                                                   ")"))))) +
          xlab(expression(paste('Change in larval amphidromy ',
                                (alpha[italic(initial)] - alpha[italic(new)])))) +
          labs(fill = expression(atop('Change in\nadult residency', 
                                      R[italic(new)] - R[italic(initial)]))) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)), # +
      # annotate("text", x = 0.4, y = 0.95*2.5, label = "b") +
      # geom_text(label = 'b', x = -0.65, y = 0.99*3, color = 'black') +
      # coord_cartesian(clip = 'off'),
      vp = vplayout(2,1))
grid.text(label = 'a', x = 0.06, y = 0.98, vp = vplayout(1,1))
grid.text(label = 'b', x = 0.06, y = 0.98, vp = vplayout(2,1))
dev.off()



# Figure S1 ---------------------------------------------------------------

# Resident odds on y, S on x, colors = odds amphidromy
mypalS1 = newpal(11)
mypalS1[6] = 'black'

Ocut <- round(seq(from = -5, to = 5, by = 1),2)
ORvScOA <- ggplot(subset(calcRO, log(oddsA,10) %in% Ocut), aes(x = log(S,10), y = log(oddsR,10), color = factor(log(oddsA,10)))) + geom_line()
ORvScOA <- ORvScOA + 
  scale_colour_discrete(type=mypalS1, breaks = c(-5,-3,-1, 0, 1, 3, 5)) + 
  xlab(expression(log(italic(s[f])/italic(s[o])))) +
  ylab(expression(log(italic(R)/(1-italic(R))))) +
  labs(color=expression(log(alpha/(1-alpha)))) +
  guides(color=guide_legend(nrow=2)) +
  ylim(-5,5) +
  geom_line(y = log(0.6/(1-0.6),10), color = 'grey60', linetype = 'dashed') + 
  geom_line(y = log(0.8/(1-0.8),10), color = 'grey60', linetype = 'dashed')
ORvScOA


# Heatmap of logged changes
logRCr <- ggplot(SrAr, 
                 aes(x = logratA, y = logratS, z = logRrat, color = logRrat)) +
  geom_raster(aes(fill = logRrat)) +
  geom_contour(aes(colour = ..level..)) + 
  scale_color_gradient(low = 'white', high = 'black', guide = F)+
  scale_fill_viridis(option = 'viridis')+
  ylab(expression(paste("log", bgroup("(",
                                      frac((italic(s[f]) / italic(s[o]))[italic(new)], (italic(s[f]) / italic(s[o]))[italic(initial)]),
                                      ")")))) +
  xlab(expression(paste('log', bgroup("(",
                                      frac((alpha / (1-alpha))[italic(initial)], (alpha / (1-alpha))[italic(new)]),
                                      ")")))) +
  labs(fill = expression(paste('log', bgroup("(",
                                             frac((R / (1-R))[italic(new)], (R / (1-R))[italic(initial)]),
                                             ")"))))
logRCr <- logRCr + geom_dl(aes(label=..level.., color = ..level..),
                           method=list("visualcenter"), stat="contour") 
figfinder(logRCr)

#### Save FIGURE S.1 (Companion to fig 2)  ###
pdf("S1.pdf", width=12/2.54, height=12/2.54)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
print(ORvScOA + pubtheme +
        guides(color=guide_legend(ncol = 1)) +
        theme(legend.position = 'right'),
      vp = vplayout(1, 1))
print(logRCr + pubtheme + theme(panel.grid = element_blank()) + 
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)),
      vp = vplayout(2,1))
grid.text(label = 'a', x = 0.03, y = 0.98, vp = vplayout(1,1))
grid.text(label = 'b', x = 0.03, y = 0.98, vp = vplayout(2,1))
dev.off()


# Figure S2 Demonstrate invariance for shift from 60 to 80 ---------------------

# Make the R60-80 plots
# Given alpha
conA <- data.frame(seta)
conA <- mutate(conA, R60 = 0.6, R80 = 0.8, Sratio = (R80/(1-R80)) / (R60/(1-R60)),
               oddsA = seta/(1-seta),
               oddsR60 = R60/(1-R60),
               oddsR80 = R80/(1-R80),
               Sdiff = oddsA*(oddsR80-oddsR60))
Srat <- ggplot(conA, aes(x = seta, y = Sratio)) + geom_line() +
  ylab(expression(frac((italic(s[f]) / italic(s[o]))[80], (italic(s[f]) / italic(s[o]))[60]))) +
  xlab(expression(paste('Larval amphidromy rate ', (alpha))))
# Dashed lines showing values of each- confusing, don't include
Srat + geom_line(aes(x = seta, log(oddsR60*oddsA,10)), linetype = 'dashed')+
  geom_line(aes(x = seta, log(oddsR80*oddsA,10)), linetype = 'dashed')

# Given sh/sa
conO <- data.frame(setS)
conO <- mutate(conO, R60 = 0.6, R80 = 0.8, oddsRatio = (R80/(1-R80)) / (R60/(1-R60)),
               aDiff = (R60 / (R60*setS - R60 - setS)) - (R80 / (R80*setS - R80 - setS)))
Orat <- ggplot(conO, aes(x = log(setS,10), y = oddsRatio)) + geom_line() +
  ylab(expression(frac((alpha / (1-alpha))[60], (alpha / (1-alpha))[80]))) +
  xlab(expression(paste('Log survival ratio ', (log(italic(s[f])/italic(s[o]))))))
# Dashed lines showing values of each- confusing, don't include
Orat + geom_line(aes(x = log(setS,10), log(setS/(R60/(1-R60)),10)), linetype = 'dashed')+
  geom_line(aes(x = log(setS,10), log(setS/(R80/(1-R80)),10)), linetype = 'dashed') 


pdf("S2.pdf", width=16/2.54, height=8/2.54)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
print(figfinder(Srat + pubtheme +
                  ylim(0,3)),
      vp = vplayout(1, 1))
print(figfinder(Orat + pubtheme  +
                  ylim(0,3)) +
        theme(plot.margin = margin(t = 5, r = 10)),
      vp = vplayout(1, 2))
grid.text(label = 'a', x = 0.07, y = 0.95, vp = vplayout(1,1))
grid.text(label = 'b', x = 0.07, y = 0.95, vp = vplayout(1,2))
dev.off()


# Figure S3: Arithmetic changes -------------------------------------------

# Given alpha (we already made conA)
sDiff <- ggplot(conA, aes(x = seta, y = Sdiff)) + geom_line() +
  ylab(expression(atop('Change in survival ratio', 
                       (italic(s[f]) / italic(s[o]))[80] - (italic(s[f]) / italic(s[o]))[60]))) +
  xlab(expression(paste('Larval amphidromy ', (alpha))))
sDiff + coord_cartesian(ylim = c(0,200))

# Given s (we already have conO)
aDiff <- ggplot(conO, aes(x = log(setS,10), y = aDiff)) + 
  geom_line() +
  # ylab(expression(alpha[60]-alpha[80])) +
  xlab(expression(atop('Log survival ratio', log(italic(s[f])/italic(s[o]))))) +
  ylab(expression(atop('Change in larval amphidromy', 
                       alpha[60]-alpha[80])))
aDiff

# Print the figure, Just the change
pdf("S3.pdf", width=16/2.54, height=8/2.54)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

print(sDiff + 
        pubtheme +
        coord_cartesian(ylim = c(0, 100)),
      vp = vplayout(1,1))

print(aDiff +
        pubtheme,
      vp = vplayout(1, 2))
grid.text(label = 'a', x = 0.07, y = 0.95, vp = vplayout(1,1))
grid.text(label = 'b', x = 0.07, y = 0.95, vp = vplayout(1,2))
dev.off()

##### A more complicated version but interesting

# uses multiple axes, which conflicts with themes
# pubtheme conflicts with the axis colors, so 
# try with black and grey instead
axthemeC <- # Color the Primary axis
  theme(axis.line.y.left = element_line(color = "firebrick"), 
        axis.ticks.y.left = element_line(color = "firebrick"),
        axis.text.y.left = element_text(color = "firebrick"), 
        axis.title.y.left = element_text(color = "firebrick")) +
  #and/or secondary
  theme(axis.line.y.right = element_line(color = "grey50"), 
        axis.ticks.y.right = element_line(color = "grey50"),
        axis.text.y.right = element_text(color = "grey50"), 
        axis.title.y.right = element_text(color = "grey50"))

# Difference and the raw values
sDiffandRaw <- sDiff +
  geom_line(aes(x = seta, y = oddsR60*oddsA), linetype = 'dashed', color = 'grey50') +
  geom_line(aes(x = seta, y = oddsR80*oddsA), linetype = 'dotted', color = 'grey50') +
  # above adds the lines for the magnitudes
  scale_y_continuous(sec.axis = sec_axis(~(.), 
                                         name = expression(paste('Raw survival ratio ', 
                                                                 (italic(s[f])/italic(s[o])))))) 
sDiffandRaw + coord_cartesian(ylim = c(0,200)) + axthemeC

aDiffandRaw <- aDiff +
  # Raw value lines
  geom_line(aes(x = log(setS,10), y = (setS/(R60/(1-R60))) / (1 + (setS/(R60/(1-R60))))), 
            linetype = 'dashed', color = 'grey50')+
  geom_line(aes(x = log(setS,10), y = (setS/(R80/(1-R80))) / (1 + (setS/(R80/(1-R80))))), 
            linetype = 'dotted', color = 'grey50') +
  # set up axes
  scale_y_continuous(sec.axis = sec_axis(~(.), 
                                         name = expression(paste('Raw larval amphidromy ', 
                                                                 (alpha)))))
aDiffandRaw + axthemeC

# side by side, colored axes
pdf("S3_LR_color.pdf", width=16/2.54, height=8/2.54)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
print(sDiffandRaw + 
        geom_line(color = 'firebrick') + 
        pubtheme + axthemeC + 
        coord_cartesian(ylim = c(0, 100)) +
        annotate("text", x = 0.6, y = 25, 
                 label = '(italic(s[f])/italic(s[o]))[80]', 
                 parse = T, color = 'grey50') +
        annotate("text", x = 0.85, y = 0, 
                 label = '(italic(s[f])/italic(s[o]))[60]', 
                 parse = T, color = 'grey50'),
      vp = vplayout(1,1))
print(aDiffandRaw + 
        geom_line(color = 'firebrick') + 
        annotate("text", x = -0.8, y = 0.5, label = 'alpha[60]', parse = T, color = 'grey50') +
        annotate("text", x = 1.6, y = 0.5, label = 'alpha[80]', parse = T, color = 'grey50') +
        pubtheme + axthemeC,
      vp = vplayout(1, 2))
grid.text(label = 'a', x = 0.07, y = 0.95, vp = vplayout(1,1))
grid.text(label = 'b', x = 0.07, y = 0.95, vp = vplayout(1,2))
dev.off()


