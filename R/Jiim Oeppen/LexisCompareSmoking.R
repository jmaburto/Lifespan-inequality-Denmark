
# Compare Lexis surface differences in life expectancy by sex and smoking ----------

# NB NB NB input data are not sorted !!!!!

# nb nb nb Only filter by age after all demographic calculations done

# NB NB NB Radix of smoking data mortality is 1000

#   Designed for abridged life tables.
#     NB NB NB in HMD age groups are constant, but year groups can vary in width
#             e.g. at start or end of series

# Requested life table data are assembled into one data frame identified by:
#   1) Country
#   2) Sex
#   3) Smoking status
#   4) Year1 Year2
#   5) Age1  Age2

#  rm and warn options ---------------------------------------
rm(list = ls(all.names = T))
# log warnings can be avoided by setting zero death counts to 0.5
#  (warn=0) warnings are stored until return
#  (warn=1) warnings are printed as they occur
#  (warn=2+) warnings are converted to errors
# NB ggplot2 generates warnings if plotting outside
#  explicit x,y, limits e.g. best-practice line
options(warn = 1)

#------ User options start here ---------------------------------------------

PCname <- Sys.info()[['nodename']]
if (PCname == "ADM-105351") {
  # Jim's desktop
  source("q:/R/workspace/ggplot2/MyTheme.r")
  #source("q:/composition/HMD functions.r", echo = T)
  #source("q:/composition/CoDa Plot functions.r", echo = TRUE)
  data.folder <-
    graph.folder <- file.path("q:/R/workspace/CancerProject/")
} else if (PCname == "JIMSLAPTOP" |
           PCname == "JIMSDELL") {
  # Jim's laptops
  source("c:/users/Jim Oeppen/R/workspace/ggplot2/MyTheme.r")
  #source("c:/Users/Jim Oeppen/composition/HMD functions.r", echo = T)
  #source("c:/Users/Jim Oeppen/composition/CoDa Plot functions.r", echo = TRUE)
  data.folder <-
    graph.folder <-
    file.path("c:/Users/Jim Oeppen/R/workspace/CancerProject")
}

# All countries in Data Set
# Countries <- c("Australia",      "Austria",        "Belgium",        "Canada",
#                "Denmark",        "Finland",        "France",         "Hungary" ,
#                "Iceland",        "Italy",          "Japan",          "Netherlands",
#                "New Zealand",    "Norway",         "Portugal" ,      "Spain",
#                "Sweden",         "Switzerland",    "United Kingdom", "United States of America")

# Form list of country pairwise contrasts by Country, Sex, and Smoking
# SMoking values: S = Smokers, N = Non-Smokers, A = All

# Contrasts <- read.table(header = TRUE, as.is = TRUE, text = "
#   Country1 Sex1   Smoking1     Country2 Sex2 Smoking2
#   Denmark      Female     N    Denmark       Female     S
#   Denmark      Male       N    Denmark       Male       S
#                         ")

Contrasts <- read.table(
  header = TRUE,
  as.is = TRUE,
  text = "
  Country1 Sex1   Smoking1     Country2 Sex2 Smoking2
  Sweden Female     A          Denmark   Female    A
  "
)

if (nrow(Contrasts) > 1) {
  cat("\n***** This program only works for 1 contrast")
  stop()
}

# Choose plotting variable: life expectancy = "ex";
#                           partial life expectancy = "pex"; NB not implemented yet
#                           mortality rate = "mx"
#                           Arriaga decomposition = "exdel"

PlotVar <- "exdel"

if (PlotVar != "exdel") {
  cat("\n***** This program only works for PlotVar = exdel")
  stop()
}

# Choose Plot Type: "AbsDiff", "PercDiff", "RR" [RR only set up for mx]
PlotType <- "PercDiff"

# set limits to be applied to all data
Year.start <- 1955
Year.stop <- 2011

Age.start <- 50
Age.stop <- 85 # closing age of last age group to be plotted

cohort.lines <- c(1919, 1939)

Age.closeLT <- 100 # Set age to close life table

# Midpoint OK for 85+ if life table closed at 100, but not higher
# HMD DNK 1950-2010. Female a(85+) = 4.5 to 6.5, Male a(85+) = 4.5 to 5.5
ax.open <- 6 # Assumed ax table with open interval 85 - 100

# names of data files by sex
male.file <- "dM0allyears.csv"
female.file <- "dF0allyears.csv"

mx.radix <- 1000

# Libraries etc. ----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(directlabels)
library(RColorBrewer)

# Functions ---------------------------------------------------------------

LT.Abridged <- function(mx, ax, ages, radix = 1.0) {
  # Checked with Austria, males, 1992.	From Preston et al. (2001) book
  nrows <- length(mx)
  n <-
    c(rev(-diff(rev(ages))), NA) # widths of age-groups; open interval = NA
  
  qx <- (n * mx) / (1 + (n - ax) * mx)
  qx[nrows] <- 1.0
  px <- 1 - qx
  lx.long <- c(radix, cumprod(px) * radix)
  lx <- head(lx.long,-1)
  dx <- rev(diff(rev(lx.long)))
  Lx <- n * (lx - dx) + ax * dx
  Lx[nrows] <- lx[nrows] / mx[nrows]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  
  return(
    data.frame(
      Age = ages,
      n = n,
      ax = ax,
      mx = mx,
      qx = qx,
      px = px,
      lx = lx,
      dx = dx,
      Lx = Lx,
      Tx = Tx,
      ex = ex
    )
  )
}

ArriagaDecomp <- function(age, lx1, Lx1, lx2, Lx2) {
  # Preston et al. (2001) Box 3.4, p.65
  # Special case for open interval
  # Eqn. 3.11
  Tx1 <- rev(cumsum(rev(Lx1)))
  Tx2 <- rev(cumsum(rev(Lx2)))
  
  del <- (lx1 / lx1[1]) * (Lx2 / lx2 - Lx1 / lx1) +
    (c(tail(Tx2,-1), 0) / lx1[1]) *
    (lx1 / lx2 - c(tail(lx1,-1), 0) / c(tail(lx2,-1), 0))
  
  # Special case for open interval
  nrows <- length(lx1)
  del[nrows] <- (lx1[nrows] / lx1[1]) *
    (Tx2[nrows] / lx2[nrows] - Tx1[nrows] / lx1[nrows])
  data.frame(age = age, Arriaga.del = del)
  
}

# Filter data from data frame and check year limits

# NB NB NB input data are not sorted !!!!!

FilterCheckYrs <-
  function(Filter.Country,
           Filter.Sex,
           Filter.Smoking,
           in.data,
           Age.closeLT,
           ax.open,
           mx.radix,
           Year.start,
           Year.stop) {
    # select data
    DD <-
      in.data %>% filter(Filter.Country == Country, toupper(Filter.Sex) == Sex)
    
    # Check Year limits -------------------------------------------------------
    
    if (Year.start < min(DD$year)) {
      cat("\nYear.start = ", Year.start, "  Data start = ", DD$year[1])
      stop(paste("Year.start before start of data for", Filter.Country))
    }
    
    if (Year.stop > max(DD$year)) {
      cat("\nYear.stop = ", Year.stop, "  Data start = ", tail(DD$year, 1))
      stop(paste("Year.stop after end of data for", Filter.Country))
    }
    
    # Parse Age1, Age2 from Agegrp and sort the data
    t <- str_split_fixed(DD$Agegrp, "-", 2) # split into two columns
    t[which(t[, 2] == ""), 2] <- Age.closeLT # close the open interval
    
    DD <-
      DD %>% mutate(Age1 = parse_number(t[, 1]), Age2 = parse_number(t[, 2])) %>%
      arrange(year, Age1)
    
    # select mx variable
    if (Filter.Smoking == "S") {
      DD$mx <- DD$mortS
    } else if (Filter.Smoking == "N") {
      DD$mx <- DD$mortNS
    } else if (Filter.Smoking == "A") {
      DD$mx <- DD$mortAll
    } else {
      stop(paste("Smoking indicator must be S, N, or A; but it is ", Filter.Smoking))
    }
    
    # calculate the abridged life table
    ax <- with(DD, (sort(unique(Age2)) + 1 - sort(unique(Age1))) / 2)
    ax[length(ax)] <- ax.open
    
    LT <-
      DD %>% group_by(year) %>% do(LT.Abridged(.$mx / mx.radix, ax, .$Age1))
    LT <- LT %>% select(year, Age, lx, Lx, ex)
    
    DD <- left_join(DD, LT, c("year" = "year", "Age1" = "Age"))
    
    DD <- DD %>% select(ID, Country, Sex, year, Agegrp, Age1, Age2, mortAll, mortS,
                        mortNS,  mx, lx, Lx,  ex) %>%
      mutate(Year1 = year,
             Year2 = year,
             Smoking = Filter.Smoking)
    DD
  }

#tt <- FilterCheckYrs("Denmark", "Female", "A", in.data, Age.closeLT, ax.open, mx.radix,
#                           Year.start, Year.stop)

MakeData <-
  function(Contrasts,
           in.data,
           Age.closeLT,
           ax.open,
           mx.radix,
           Year.start = 1751,
           Year.stop = format(Sys.time(), "%Y"),
           Age.start = 0,
           Age.stop = 110) {
    Data.out <- data.frame(
      Year1 = integer(0),
      Year2 = integer(0),
      Age1 = integer(0),
      Age2 = integer(0),
      Country1 = character(0),
      Sex1 = character(0),
      Smoking1 = character(0),
      mx1 = numeric(0),
      lx1 = numeric(0),
      Lx1 = numeric(0),
      ex1 = numeric(0),
      Country2 = character(0),
      Sex2 = character(0),
      Smoking2 = character(0),
      mx2 = numeric(0),
      lx2 = numeric(0),
      Lx2 = numeric(0),
      ex2 = numeric(0),
      Diff = numeric(0),
      PDiff = numeric(0),
      Label = character(0)
    )
    
    for (i in 1:nrow(Contrasts)) {
      # filter data for life tables --------------------------------------------
      
      LT1 <-
        FilterCheckYrs(
          Contrasts$Country1[i],
          Contrasts$Sex1[i],
          Contrasts$Smoking1[i],
          in.data,
          Age.closeLT,
          ax.open,
          mx.radix,
          Year.start,
          Year.stop
        )
      
      LT2 <-
        FilterCheckYrs(
          Contrasts$Country2[i],
          Contrasts$Sex2[i],
          Contrasts$Smoking2[i],
          in.data,
          Age.closeLT,
          ax.open,
          mx.radix,
          Year.start,
          Year.stop
        )
      
      # select the data by Period ----------------------------------------------
      LT1 <- LT1 %>% filter(between(Year1, Year.start, Year.stop),
                            between(Year2, Year.start, Year.stop))
      
      LT2 <- LT2 %>% filter(between(Year1, Year.start, Year.stop),
                            between(Year2, Year.start, Year.stop))
      
      if (!identical(LT1$Year, LT2$Year) |
          !identical(LT1$Age, LT2$Age))
        stop(
          paste(
            "Years and/or Ages are not identical for",
            Contrasts$Country1[i],
            " and ",
            Contrasts$Country2[i]
          )
        )
      
      # # construct the pairwise contrast
      td <- data.frame(
        Year1 = LT1$Year1,
        Year2 = LT1$Year2,
        Age1 = LT1$Age1,
        Age2 = LT1$Age2,
        Country1 = LT1$Country,
        Sex1 = str_to_title(LT1$Sex),
        Smoking1 = LT1$Smoking,
        mx1 = LT1$mx,
        lx1 = LT1$lx,
        Lx1 = LT1$Lx,
        ex1 = LT1$ex,
        Country2 = LT2$Country,
        Sex2 = str_to_title(LT2$Sex),
        Smoking2 = LT2$Smoking,
        mx2 = LT2$mx,
        lx2 = LT2$lx,
        Lx2 = LT2$Lx,
        ex2 = LT2$ex
      ) %>%
        mutate(#Label="All mortality"
          Label = "Non-smoking attributable mortality"
          # Label=paste(Country1,", ",
          #                  tolower(str_sub(Sex1, 1, 1)), ", ", Smoking1, " vs. ",
          #                  Country2,", ",
          #                  tolower(str_sub(Sex2, 1, 1)), ", ", Smoking2, sep="")
          )
          Data.out <- rbind(Data.out, td)
    }
    
    # Arriaga decomposition
    decomp <- Data.out %>% group_by(Year1) %>%
      do(ArriagaDecomp(.$Age1, .$lx1, .$Lx1, .$lx2, .$Lx2))
    
    Data.out <-
      left_join(Data.out, decomp, c("Year1" = "Year1", "Age1" = "age"))
    Data.out
  }

# Create data frame to add cohort lines to plots, possibly faceted by type --

Cohorts.df <- function(cohorts,
                       period.start,
                       period.stop,
                       age.start,
                       age.stop,
                       types = NA) {
  if (is.na(types[1])) {
    types <- rep("??", length(cohorts))
  }
  cdf <-
    data.frame(
      Cohort = integer(0),
      Period = integer(0),
      Age = integer(0),
      Type = character(0)
    )
  for (t in types) {
    for (c in cohorts) {
      # does the cohort cross the Lexis surface?
      if (c + age.stop > period.start &
          c + age.start < period.stop) {
        # define first point of cohort line
        if (period.start - c > age.start) {
          period <- period.start
          age <- period.start - c
        } else {
          period <- c + age.start
          age <- age.start
        }
        cdf <-
          rbind(cdf,
                data.frame(
                  Cohort = c,
                  Period = period,
                  Age = age,
                  Type = t
                ))
        
        # define second point of cohort line
        if (c + age.stop > period.stop) {
          period <- period.stop
          age <- period.stop - c
        } else {
          period <- c + age.stop
          age <- age.stop
        }
        cdf <-
          rbind(cdf,
                data.frame(
                  Cohort = c,
                  Period = period,
                  Age = age,
                  Type = t
                ))
      }
    }
  }
  cdf
}

# Check plot var and type ----------------------------------------------------

if (!PlotVar %in% c("ex", "pex", "mx", "exdel")) {
  stop(paste(
    "Plot Variable must be \"ex\", \"pex\", \"mx\", or \"exdel\"; but it is ",
    PlotVar
  ))
}

if (!PlotType %in% c("AbsDiff", "PercDiff", "RR")) {
  stop(paste(
    "Plot Type must be \"AbsDiff\", \"PercDiff\", or \"RR\"; but it is ",
    PlotType
  ))
}

if (PlotVar == "pex" & PlotType == "RR") {
  stop("\nChoosing PlotVar = \"pex\" & PlotType = \"RR\" does not make sense")
}

# Get all the data --------------------------------------------------------

fpath.male <- file.path(data.folder, male.file)
fpath.female <- file.path(data.folder, female.file)

Male.data <-
  read.csv(fpath.male,
           as.is = TRUE,
           header = TRUE,
           na.strings = "NA") %>%
  mutate(Sex = "MALE")

Female.data <-
  read.csv(
    fpath.female,
    as.is = TRUE,
    header = TRUE,
    na.strings = "NA"
  ) %>%
  mutate(Sex = "FEMALE")

in.data <- rbind(Male.data, Female.data)

# Construct the Contrasts -------------------------------------------------

# nb nb nb Only filter by age after all demographic calculations done

DD <- MakeData(
  Contrasts,
  in.data,
  Age.closeLT,
  ax.open,
  mx.radix,
  Year.start,
  Year.stop,
  Age.start,
  Age.stop
)

# check the sums of the changes in ex
qq <-
  DD %>% group_by(Year1) %>% summarise(e0change = sum(Arriaga.del))

DD %>% filter(Age1 == Age.start) %>% mutate(error = (ex2 - ex1) - qq$e0change) %>%
  select(Year1, error)

# Checking why missing years for Arriaga calculation: mortS 85- often NA.
# DD %>% filter(Year1 == 1992)
# Female.data %>% select(Country, year, Agegrp, mortAll, mortNS, mortS) %>%
#   filter(Country %in% c("Denmark", "Sweden"), Agegrp == "85-")

DD <- DD %>% filter(Age1 < Age.stop)

saveRDS(DD, file.path(data.folder, "A_Abridged.rds"))

# Plot Specific Operations ------------------------------------------------

if (PlotVar == "mx") {
  title.phrase <- "Mortality Risk"
  V1 <- DD$mx1
  V2 <- DD$mx2
} else if (PlotVar == "ex") {
  title.phrase <- "Period Life Expectancy"
  V1 <- DD$ex1
  V2 <- DD$ex2
} else if (PlotVar == "pex") {
  title.phrase <- "Period Partial Life Expectancy"
  V1 <- DD$pex1
  V2 <- DD$pex2
} else if (PlotVar == "exdel") {
  DD$Gap <-
    -DD$Arriaga.del # function compares second life table to first
  title <- "Decomposition of Life Expectancy Difference"
  Legend.Title <- "Years\n"
  contour.breaks <- seq(-0.2, 0.8, 0.1)
}

if (!PlotVar == "exdel") {
  if (PlotType == "AbsDiff") {
    DD$Gap <- V1 -  V2
    title <- paste("Absolute Difference in ", title.phrase, sep = "")
    Legend.Title <- "Years"
    contour.breaks <- seq(-10, 10, 1)
  } else if (PlotType == "PercDiff") {
    DD$Gap <- 100 * ((V1 - V2) / V2)
    title <-
      paste("Percentage Difference in ", title.phrase, sep = "")
    contour.breaks <- seq(-50, 100, 5)
    Legend.Title <- "%"
  } else if (PlotType == "RR") {
    # therefore PlotVar = "mx"
    DD$Gap <- V1 / V2
    title <- "Mortality Risk Ratio"
    contour.breaks <- seq(-5, 5, 0.1)
    Legend.Title <- "RR"
  }
}

# Change the order of the facets: default is alphabetic order

# DD$Label <- factor(DD$Label,
#                        levels = c("DNK,F vs. DNK,M", "NOR,F vs. NOR,M",
#                                   "SWE,F vs. SWE,M", "FIN,F vs. FIN,M"))

# Contours on raster plot -------------------------------------------------

# set up minimal data frame for cohorts and their labels for geom_dl
types.list <- unique(DD$Label)

# set up minimal data frame for cohorts and their labels for geom_dl
Cohdf <-
  Cohorts.df(cohort.lines,
             types = unique(DD$Label),
             Year.start,
             Year.stop + 1,
             Age.start,
             Age.stop) %>% mutate(Gap = NA)

# note x and y are centre of cells to match definition of contour
p <- ggplot(DD, aes(x = Year1 + 0.5, y = Age1 + 2.5, z = Gap)) +
  facet_grid(. ~ Label) +
  coord_equal() + # Lexis surface assumption of square grid
  geom_rect(aes(
    xmin = Year1,
    xmax = Year2 + 1,
    ymin = Age1,
    ymax = Age2 + 1,
    fill = Gap
  )) +
  
  # geom_vline(xintercept=1980, colour="blue", size=1) +
  # geom_hline(yintercept=65, colour="blue", size=1) +
  # add the cohort lines before the contours
  geom_line(
    data = Cohdf,
    aes(x = Period, y = Age, group = Cohort),
    colour = "darkred",
    size = 1.5
  ) +
  # direct label the cohort lines
  #  "first.points", rot=45, vjust = 0, hjust=0
  #  "last.points", rot=45, vjust = 0, hjust=1
  # Added 1 year to Age to lift cohort label above cohort line
  geom_dl(
    data = Cohdf,
    aes(x = Period, y = Age + 1, label = Cohort),
    method = list(
      "first.points",
      rot = 45,
      vjust = 0,
      hjust = 0,
      cex = 1.5,
      colour = "darkred"
    )
  ) +
  
  # stat_contour(aes(colour=..level..), colour="black", breaks= contour.breaks) +
  # geom_dl(aes(label=..level..), colour="black", breaks= contour.breaks,
  #         #method="bottom.pieces", stat="contour") +
  #         method="top.pieces", stat="contour") +
  stat_contour(
    data = DD,
    aes(colour = ..level..),
    colour = "black",
    breaks = contour.breaks
  ) +
  geom_dl(
    aes(label = ..level..),
    colour = "black",
    breaks = contour.breaks,
    #method="bottom.pieces", stat="contour") +
    method = "top.pieces",
    stat = "contour"
  ) +
  # scale_fill_gradientn(colours=MyColours, name=Legend.Title, limits=range(contour.breaks),
  #                      breaks= contour.breaks) +
  scale_fill_gradient2(
    midpoint = 0.0,
    #low="red", mid="white", high="blue",
    low = "darkblue",
    mid = "ivory",
    high = "red",
    space = "Lab",
    #name=Legend.Title,
    name = "Difference\nin years\n",
    breaks = contour.breaks,
    limits = range(contour.breaks)
  ) +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  
  scale_colour_continuous(guide = "none") +
  #scale_fill_distiller(palette = "Spectral", name="Years", breaks= contour.breaks) +
  # scale_fill_gradient2(low = "steelblue", mid="white", high = "red",
  #                      name=Legend.Title, breaks= contour.breaks) +
  # scale_fill_gradient2(low = "steelblue", mid="white", high = "red",
  #                      name=Legend.Title) +
  #scale_colour_continuous(guide="none") +
  # some space for labels at edges - depends on choice of position
  scale_x_continuous(
    limits = c(Year.start, Year.stop + 1),
    breaks = seq(1750, 2015, 10),
    expand = c(0.03, 0)
  ) +
  scale_y_continuous(
    limits = c(Age.start, Age.stop),
    breaks = seq(55, 85, 10),
    expand = c(0, 0)
  ) +
  MyTheme(18) +
  theme(legend.key.height = unit(3, "cm")) +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  theme(axis.title.x = element_text(margin = margin(5, 0, 0, 0))) +
  # theme(panel.margin.x=unit(1, "lines")) + # more horizontal space between facets
  #ggtitle(title) +
  theme(plot.title = element_text(size = 32, margin = margin(0, 0, 10, 0))) +
  labs(x = "Year", y = "Age")

p

fname <- with(
  Contrasts,
  paste(
    PlotVar[1],
    PlotType[1],
    substr(Country1[1], 1, 3),
    substr(Sex1[1], 1, 1),
    Smoking1[1],
    "vs",
    substr(Country2[1], 1, 3),
    substr(Sex2[1], 1, 1),
    Smoking2[1],
    sep = "_"
  )
)

print(p) # seems to expand plot
ggsave(
  file.path(graph.folder, paste(fname, ".pdf", sep = "")),
  #width = 20, height = 20, units = "cm")
  #width = 21, height = 29.7, units = "cm")  # A4 portrait 21 x 29.7
  width = 29.7,
  height = 21,
  units = "cm"
)  # A4 landscape
