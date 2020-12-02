#Produces a few plots of interest about Dominion League history
#Sections:
#data prep from history
#players per season
#player history
#transition percentages
#top of league power

##################################################
#data prep from history
##################################################

library(plyr)
library(jsonlite)
library(ggplot2)

history = fromJSON(url("https://raw.githubusercontent.com/randomtruffles/Dominion_League/master/_data/friendly_league_history.json"))[[1]]
#preprocessed version
history = fromJSON(url("https://raw.githubusercontent.com/randomtruffles/Dominion_League/master/_data/chart_history.json"))
history$place = as.numeric(history$place)
history$wins = as.numeric(history$wins)
history$losses = as.numeric(history$losses)
history$season = as.numeric(history$season)

reduced = list()

#get divisions, names, places
for (i in 1:length(history)) {
  division = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {rep(div$name, length(div$members))}})))
  player = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$name})}})))
  place = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$rank})}})))
  wins = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$wins})}})))
  losses = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$losses})}})))
  pct = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$pct})}})))
  standingsColor = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$color})}})))
  #switch top 2 for championship
  if (tolower(player[1]) != history[[i]]$champion) {
    player[1:2] = player[2:1]
    wins[1:2] = wins[2:1]
    losses[1:2] = losses[2:1]
    pct[1:2] = pct[2:1]
    standingsColor[1:2] = standingsColor[2:1]
  }
  reduced[[i]] = data.frame(player, tier = sapply(division, substr, 1, 1), division, place, wins, losses, pct, standingsColor, stringsAsFactors = FALSE)
}

names(reduced) = names(history)

history = reduced[[1]]
for (frame in reduced[2:length(reduced)]) {
  history = rbind(history, frame)
}

#get season numbers
history$season = as.integer(rep(names(reduced), sapply(reduced, (function(s) {length(s$player)}))))
history = history[order(history$season),]

#name change not in history
history$player[history$player == "The Do-Operator"] = "Anders"

#fix capitalization inconsistencies
proper = c("Dan Brooks", "ceviri", "squirezucco", "PerryGreen", "LimeTime", "hockeysemlan", "LaLight", "Hannibal6", "Awaclus", "IDontPlayThisGame", "ROGAVKA", "ephesos", "malmerung", "The Jesters Girl", "InconspicuousPanda", "Bonxie", "turtlegods", "Hackibacki", "Napoleon1805", "Makqvist", "Hugovj", "Crazyjok3r", "GeoLib", "PitrPicko", "MarkowKette", "JP Gerber")
for (p in proper) {
  history$player[tolower(history$player) == tolower(p)] = p
}

remove(reduced, frame, division, place, player, wins, losses, pct, standingsColor, proper)

##################################################
#visualizations
##################################################

#league division colors
cols = c("A" = "#FF00FF", "B" = "#9900FF", "C" = "#0000FF", "D" = "#4A85E8", "E" = "#00FFFF", "F" = "#00FF00", "G" = "#FFFF00", "H" = "#FF9800", "I" = "#FF0000", "J" = "#980000")

##################################################
#players per season - also acts as a background for player history
##################################################

#produce table of player counts by tier per season

counts = ddply(history, c("season","tier"), summarize, divisions = nunique(division), players = length(player))
#preprocessed version
#counts = fromJSON(url("https://raw.githubusercontent.com/randomtruffles/Dominion_League/master/_data/chart_counts.json"))
#counts$season = as.numeric(counts$season)
#counts$divisions = as.numeric(counts$divisions)
#counts$players = as.numeric(counts$players)
#counts$lfrac = as.numeric(counts$lfrac)


back = function(uptier = "J", from = 1, to = max(counts$season), prop = FALSE, oldE = FALSE) {
  
  ts = LETTERS[10:1]
  
  inforange = (counts$tier <= uptier) & (counts$season >= from) & (counts$season <= to)
  if (oldE) {
    inforange = inforange & !((counts$tier == "E") & (counts$season >= 27))
  }
  
  if (prop) {
    return (ggplot() + geom_col(data = counts[inforange,], mapping = aes(x = season, y = lfrac, fill = factor(tier, ts))) + scale_fill_manual(values = cols) + ylim(1,0) + guides(fill = guide_legend(reverse = TRUE)))
  } else {
    maxplayers = max(daply(counts[inforange,], "season", function(df) sum(df$players)))
    return (ggplot() + geom_col(data = counts[inforange,], mapping = aes(x = season, y = players, fill = factor(tier, ts))) + scale_fill_manual(values = cols) + ylim(maxplayers,0) + guides(fill = guide_legend(reverse = TRUE)))
  }
}

##################################################
#player history
##################################################

pChart = function(player, from = NULL, to = NULL, prop = FALSE, full = FALSE) {
  
  if (prop) {full = TRUE}
  
  phist = history[history$player == player,]
  
  if (is.null(from)) {from = min(phist$season)}
  if (is.null(to)) {to = max(phist$season)}
  
  phist = phist[(phist$season >= from) & (phist$season <= to),]
  phist$champ = (phist$tier == "A") & (phist$place == 1)
  phist = phist[order(phist$season),]
  
  if (prop) {
    phist$placement = sapply(phist$season, function(s) {
      base = sum(counts$lfrac[(counts$season == s) & (counts$tier < phist$tier[phist$season == s])])
      indiv = sum((history$season == s) & (history$division == phist$division[phist$season == s]))
      intier = counts$lfrac[(counts$season == s) & (counts$tier == phist$tier[phist$season == s])]
      base + (phist$place[phist$season == s] - 0.5)*intier/indiv
    })
  } else {
    phist$placement = sapply(phist$season, function(s) {
      base = sum(counts$players[(counts$season == s) & (counts$tier < phist$tier[phist$season == s])])
      indiv = sum((history$season == s) & (history$division == phist$division[phist$season == s]))
      intier = counts$players[(counts$season == s) & (counts$tier == phist$tier[phist$season == s])]
      base + (phist$place[phist$season == s] - 0.5)*intier/indiv
    })
  }
  
  if (full) {
    plt = back(from = from, to = to, prop=prop)
  } else {
    high = max(phist$tier)
    oldE = FALSE
    if (high == "E") {
      oldE = !any((phist$tier == "E") & (phist$season >= 27))
    }
    plt = back(uptier = high, from = from, to = to, oldE = oldE)
  }
  
  cuts = c(0, which(diff(phist$season) > 1), length(phist$season))
  for (i in 2:length(cuts)) {
    plt = plt + geom_line(data = phist[(cuts[i-1]+1):cuts[i],], mapping = aes(x = season, y = placement), size = 0.5)
  }
  
  plt = plt + geom_point(data = phist, mapping = aes(x = season, y = placement, shape = champ, size = champ), fill = "yellow") + scale_shape_manual(values = c(15,21)) + scale_size_manual(values = c(3,4)) + guides(shape = FALSE, size = FALSE)
  
  plt = plt + ggtitle(paste("League History:", player)) + labs(fill = "Tier") + xlab("Season") + ylab("Position") + theme(axis.text.y = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size = 18))

  plt
}

#find players with more than 1 season
PsofInterest = unique(history$player)[sapply(unique(history$player), function(p) {length(history$player[history$player == p]) > 1})]

#save chart images for those players
a_ply(PsofInterest, 1, function(p) {
  pChart(p)
  ggsave(paste0("./Player Charts/", p, ".png"), height = 8, width = 11)
})

##################################################
#transition percentages
##################################################

numAt = sapply(unique(history$tier), function(t) {length(unique(tolower(history$player[history$tier == t])))})
numStay = sapply(unique(history$tier), function(t) {length(unique(tolower(history$player[(history$tier == t) & (history$place < 5)])))})

transitions = data.frame(from = rep(c("any", LETTERS[2:8]), c(8,1:7)), to = c(LETTERS[1:8], unlist(lapply(1:7, function(n) LETTERS[1:n]))))

count = ddply(transitions, c("from", "to"), function(df) {
  if (df$from == "any") {return (numAt[df$to])}
  sum(sapply(unique(history$player[history$tier == df$from]), function (p) {
    min(history$season[(history$player == p) & (history$tier == df$from)]) < max(history$season[(history$player == p) & (history$tier == df$to)])
  }))
})
names(count)[3] = "v"

transitions$count = count$v
rm(count)

stayFirst = ddply(transitions, c("from", "to"), function(df) {
  if (df$from == "any") {return (numStay[df$to])}
  sum(sapply(unique(history$player[history$tier == df$from]), function (p) {
    firstSeasonUp = min(history$season[(history$player == p) & (history$tier == df$to) & (history$season > min(history$season[(history$player == p) & (history$tier == df$from)]))])
    if (firstSeasonUp > max(history$season)) {return (FALSE)}
    history$place[(history$player == p) & (history$season == firstSeasonUp)] < 5
  }))
})
names(stayFirst)[3] = "v"

transitions$stayFirst = stayFirst$v
rm(stayFirst)

transitions = rbind(data.frame(from = "any", to = "any", count = length(unique(history$player)), stayFirst = length(PsofInterest)), transitions)
transitions$promoteFrac = transitions$count/rep(transitions$count[transitions$from == "any"], c(9,0:7))
transitions$stayFrac = transitions$stayFirst/rep(transitions$count[transitions$from == "any"], c(9,0:7))
transitions = transitions[nrow(transitions):1,]

ggplot(data = transitions[(transitions$from != "any") & (transitions$from < "G"),]) + geom_col(mapping = aes(x = factor(from, LETTERS[10:1]), y = promoteFrac, fill = to), position = position_dodge2(preserve = "single", reverse = TRUE), width = 1) + 
  geom_errorbar(mapping = aes(x = from, ymin = stayFrac, ymax = stayFrac, color = rep(paste0(strwrap("stayed at or above for more than one season after first promotion", 20), collapse = "\n"),15)), position = position_dodge2(preserve = "single", reverse = TRUE), width = 1) +
  scale_fill_manual(values = cols) + scale_color_manual(values = "black") + guides(fill = guide_legend(title = "Tier Reached", order = 1), color = guide_legend("")) + xlab("Originating Tier") + ylab("Proportion of Players") + ggtitle("Difficulty of Moving Up")

ggsave("./transitions.png", height = 8, width = 11)

##################################################
#top of league power
##################################################

APlayers = sort(unique(history$player[(history$tier == "A") | ((history$tier == "B") & (history$place == 1) & (history$season == 42))]))
tophist = history[history$player %in% APlayers,]

#function giving points for a season
seaspoint = function(player, season) {
  interest = (tophist$player == player) & (tophist$season == season) & (tophist$tier < "C")
  if (!any(interest)) {return (-1)}
  numPl = sum(history$division[(history$season == season)] == history$division[(history$player == player) & (history$season == season)])
  pointMap = c(
    A1 = 10, A2 = 5, A3 = 3, A4 = 3, A5 = 1, A6 = 1,
    B1 = 2, B2 = 0, B3 = 0, B4 = 0, B5 = -2, B6 = -2
  )
  if (numPl <= 6) {
    return (unname(pointMap[paste0(tophist$tier[interest],tophist$place[interest])]))
  } else { #we need to adjust for 7 player division
    r6equiv = tophist$place[interest]
    if (r6equiv > 4) {r6equiv = r6equiv - 1}
    return (unname(pointMap[paste0(tophist$tier[interest],r6equiv)]))
  }
}

tophist$seaspoints = aaply(tophist, 1, function(r) {seaspoint(r$player, r$season)}, .expand = F)
s1Ap = history$player[(history$season == 1) & (history$tier == 'A')]
coefs = ddply(tophist, "player", function(olddf) {
  df = data.frame(player = olddf$player, season = olddf$season, points = olddf$seaspoints)
  seasons = NULL
  for (s in 1:42) {if (!(s %in% df$season)) {seasons = c(seasons, s)}}
  if (length(seasons)) {df = (rbind(df, data.frame(player = df$player[1], season = seasons, points = 0)))}
  df = df[order(df$season),]
  ma = c(1,0.9,0.7,0.4)
  coef = as.numeric(filter(c(numeric(length(ma)-1),df$points), ma, "convolution", 1))
  df$coef = sapply(coef[!is.na(coef)], function(n) {max(n,0)})
  if (df$player[1] %in% s1Ap) {df = rbind(data.frame(player = df$player[1], season = 0, points = 0, coef = 1), df)}
  else {df = rbind(data.frame(player = df$player[1], season = 0, points = 0, coef = 0), df)}
  df
})
coefsums = ddply(coefs, "season", function(df) {sum(df$coef)})
coefs$prop = aaply(coefs, 1, function(r) {r$coef/coefsums$V1[coefsums$season == r$season]},.expand = F)
coefs$prop[coefs$prop < 0.02] = 0
propsums = ddply(coefs, "season", function(df) {sum(df$prop)})
coefs$prop = aaply(coefs, 1, function(r) {r$prop/propsums$V1[propsums$season == r$season]},.expand = F)

chartNeighbors = lapply(APlayers, function(p) {character()})
names(chartNeighbors) = APlayers
for (season in 0:42) {
  names = coefs$player[(coefs$prop > 0) & (coefs$season == season)]
  for (i in 1:length(names)) {
    if (i==1) {
      chartNeighbors[[names[i]]] = c(chartNeighbors[[names[i]]], names[i+1])
    } else if (i==length(names)) {
      chartNeighbors[[names[i]]] = c(chartNeighbors[[names[i]]], names[i-1])
    } else {
      chartNeighbors[[names[i]]] = c(chartNeighbors[[names[i]]], names[i+1])
      chartNeighbors[[names[i]]] = c(chartNeighbors[[names[i]]], names[i-1])
    }
  }
}
chartNeighbors = lapply(chartNeighbors, unique)
chartNeighbors = chartNeighbors[order(sapply(chartNeighbors, function(p) {length(p)}), decreasing = T)]

chartSeasonShare = lapply(APlayers, function(p) {character()})
names(chartSeasonShare) = APlayers
for (season in 0:42) {
  names = coefs$player[(coefs$prop > 0) & (abs(coefs$season - season) < 2)]
  for (i in 1:length(names)) {
    chartSeasonShare[[names[i]]] = c(chartSeasonShare[[names[i]]], names)
  }
}
chartSeasonShare = lapply(chartSeasonShare, unique)
chartSeasonShare = chartSeasonShare[order(sapply(chartSeasonShare, function(p) {length(p)}), decreasing = T)]

#functions since color picking will be in HSL color space
HSLtoHex = function(hsl) {
  if (any(is.na(hsl))) {return("#000000")}
  hue = (hsl[1] %% 240)/40
  sat = hsl[2]/240
  lgt = hsl[3]/240
  
  chroma = sat * (1 - abs(2 * lgt - 1))
  X = chroma * (1 - abs(-1 + hue %% 2))
  
  if (hue < 1) {
    rgb = c(chroma, X, 0)
  } else if (hue < 2) {
    rgb = c(X, chroma, 0)
  } else if (hue < 3) {
    rgb = c(0, chroma, X)
  } else if (hue < 4) {
    rgb = c(0, X, chroma)
  } else if (hue < 5) {
    rgb = c(X, 0, chroma)
  } else {
    rgb = c(chroma, 0, X)
  }
  
  rgb = sapply(rgb, function(k) {k+lgt-0.5*chroma})
  rgb(rgb[1], rgb[2], rgb[3])
}

HextoHSL = function(hex) {
  rgb = col2rgb(hex)/255
  V = max(rgb)
  chroma = V - min(rgb)
  lig = V-0.5*chroma
  if (chroma==0) {
    hue = 0
  } else if (V == rgb[1]) {
    hue = (rgb[2]-rgb[3])/chroma
    if (hue < 0) {hue = 6-hue}
  } else if (V == rgb[2]) {
    hue = 2+(rgb[3]-rgb[1])/chroma
  } else {
    hue = 4+(rgb[1]-rgb[2])/chroma
  }
  if ((lig == 0) | (lig == 1)) {
    sat = 0
  } else {
    sat = (V-lig)/min(lig, 1-lig)
  }
  return(c(40*hue, 240*sat, 240*lig))
}

HSLdiff = function(hsl1, hsl2) {
  a = abs(hsl1 - hsl2)
  a[1] = min(a[1], 240-a[1])
  a
}

playerCols = lapply(APlayers, function(p) {NA})
names(playerCols) = APlayers

#factor in requests
playerCols[['tracer']] = c(7,1,1)
playerCols[['xyrix']] = c(6,4,3)
playerCols[['singletee']] = c(9,4,6)
playerCols[['RTT']] = c(4,2,0)
playerCols[['SamE']] = c(9,4,3)
playerCols[['markus']] = c(0,2,3)

set.seed(8)
possibles = matrix(c(rep(0:11, each = 25), rep(0:4, each = 5, times = 12), rep(0:4, times = 60)),nrow = 300, ncol = 3)
for (i in 1:length(APlayers)) {
  player = names(chartNeighbors)[i]
  neighbors = chartNeighbors[[player]]
  #seasonShare = chartSeasonShare[[player]]
  if (is.na(playerCols[[player]]) & length(neighbors)) {
    neighborCols = aaply(neighbors[sapply(neighbors, function(p) {!is.na(playerCols[p])})],1, .fun = function(p) {playerCols[[p]]})
    #seasonShareCols = aaply(seasonShare[sapply(seasonShare, function(p) {!is.na(playerCols[p])})],1, .fun = function(p) {playerCols[[p]]})
    if (length(dim(neighborCols)) == 2) {
      playerCols[[player]] = possibles[sample((1:300)[
        apply(possibles, 1, function(r) {min(abs(r[1] - neighborCols[,1]), 12-abs(r[1] - neighborCols[,1])) > 1}) &
          apply(possibles, 1, function(r) {!isTRUE(any(sapply(playerCols, function(c) {(r[1]==c[1]) & (all(abs(r[-1]-c[-1]) < 2))})))})
        ],1),]
    } else if (length(neighborCols) == 3) {
      playerCols[[player]] = possibles[sample((1:300)[
        (sapply(abs(possibles[,1] - neighborCols[1]), function(x) min(x,12-x))>1) &
          apply(possibles, 1, function(r) {!isTRUE(any(sapply(playerCols, function(c) {(r[1]==c[1]) & (all(abs(r[-1]-c[-1]) < 2))})))})
        ],1),]
    } else {
      playerCols[[player]] = possibles[sample((1:300)[
          apply(possibles, 1, function(r) {!isTRUE(any(sapply(playerCols, function(c) {(r[1]==c[1]) & (all(abs(r[-1]-c[-1]) < 2))})))})
        ],1),]
    }
  }
}

playerCols = sapply(playerCols, function(co) {HSLtoHex(c(20,35,20) * co + c(0,95,65) + runif(3,-5,5))})

#assign requests
playerCols[['tracer']] = "#284F8F"
playerCols[['xyrix']] = "#00FFFF"
playerCols[['singletee']] = "#BC9FFC"
playerCols[['RTT']] = "#0F6419"
playerCols[['SamE']] = "#911CFD"
playerCols[['markus']] = "#DE2226"

ggplot() + geom_area(mapping = aes(x = season, y = prop, fill = player), data = coefs) + scale_fill_manual(values = playerCols) + coord_flip(xlim = c(41,0), ylim = c(1,0))
