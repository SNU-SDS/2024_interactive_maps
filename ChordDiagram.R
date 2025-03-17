# Loading Packages --------------------------------------------------------

library(devtools)
devtools::install_github("mattflor/chorddiag")
library(chorddiag)
library(readxl)
library(systemfonts)

fonts <- system_fonts()
unique(fonts$family)
# Data Preparing ----------------------------------------------------------

return <- read_xlsx("return.xlsx")
return <- as.matrix(return)

# Plot -----------------------------------------------------------------

# A vector of 4 colors for 4 groups
haircolors <- c("수도권", "강원권", "충청권", "영남권", "호남권", "제주권")
dimnames(return) <- list(
  have = haircolors,
  prefer = haircolors
)
groupColors <- c("#FF6F61", "#6B5B95", "#88B04B", "#FFA177", "#F7CAC9", "#92A8D1")

# Build the chord diagram:
p <- chorddiag(return, 
               groupColors = groupColors,
               groupnamePadding = 20
               )
p <- htmlwidgets::onRender(p, "
  function(el, x) {
    var labels = el.querySelectorAll('text');
    labels.forEach(function(label) {
      label.style.fontFamily = 'Arial';  // 원하는 글꼴로 변경
    });
  }
")
p

htmlwidgets::saveWidget(p, "return_chorddiagram.html")
