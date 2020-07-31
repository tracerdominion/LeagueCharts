#data prep from history

library(jsonlite)

history = fromJSON(url("https://raw.githubusercontent.com/randomtruffles/Dominion_League/master/_data/friendly_league_history.json"))[[1]]

reduced = list()

for (i in 1:length(history)) {
   division = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {rep(div$name, length(div$members))}})))
   player = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$name})}})))
   place = unname(unlist(lapply(history[[i]], function(div) {if (length(div) > 1) {sapply(div$members, function(pl) {pl$rank})}})))
   if (tolower(player[1]) != history[[i]]$champion) {player[1:2] = player[2:1]}
   reduced[[i]] = data.frame(player, tier = sapply(division, substr, 1, 1), division, place, stringsAsFactors = FALSE)
}

names(reduced) = names(history)

history = reduced[[1]]
for (frame in reduced[2:length(reduced)]) {
  history = rbind(history, frame)
}

history$season = as.integer(rep(names(reduced), sapply(reduced, (function(s) {length(s$player)}))))
history$player[history$player == "The Do-Operator"] = "Anders"

remove(reduced)

library(plyr)

counts = ddply(history, c("season","tier"), summarize, divisions = nunique(division), players = length(player)) 
counts$lfrac = counts$players/rep(daply(counts, "season", function(df) sum(df$players)), table(counts$season))

#visualizations

library(ggplot2)

cols = c("A" = "#FF00FF", "B" = "#9900FF", "C" = "#0000FF", "D" = "#4A85E8", "E" = "#00FFFF", "F" = "#00FF00", "G" = "#FFFF00", "H" = "#FF9800", "I" = "#FF0000", "J" = "#980000")

back = function(uptier = "J", from = 1, to = max(counts$season), prop = FALSE, oldE = FALSE) {
  
  ts = rev(LETTERS[1:10])
  
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

pChart = function(player, from = NULL, to = NULL, prop = FALSE, full = FALSE) {
  
  if (prop) {full = TRUE}
  
  phist = history[tolower(history$player) == tolower(player),]
  
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
    plt = plt + geom_line(data = phist[(cuts[i-1]+1):cuts[i],], mapping = aes(x = season, y = placement))
  }
  
  plt = plt + geom_point(data = phist, mapping = aes(x = season, y = placement, shape = champ, size = champ), fill = "yellow") + scale_shape_manual(values = c(15,21)) + scale_size_manual(values = c(3,4)) + guides(shape = FALSE, size = FALSE)
  
  plt = plt + ggtitle(paste("League History:", player)) + labs(fill = "Tier") + xlab("Season") + ylab("Position") + theme(axis.text.y = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size = 18))

  plt
}

PsofInterest = unique(history$player)[sapply(unique(history$player), function(p) {length(history$player[history$player == p]) > 1})]

a_ply(PsofInterest, 1, function(p) {
  pChart(p)
  ggsave(paste0("./Player Charts/", p, ".png"), height = 8, width = 11)
})

