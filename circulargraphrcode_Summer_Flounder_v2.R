#Geret DePiper
#Code to generate circlize Summer Flounder conceptual models
#July 25, 2019

PKG <- c("foreign","foodweb","sna","DiagrammeR","circlize","RColorBrewer","QPress")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

#Set your working directory to the folder containing the "For class" folder, on my computer it's:
setwd("X:/gdepiper/MAFMC EAFM/Summer_Flounder")

edges <- model.dia("Summer_Flounder_July22_2019.dia")

## Examine unweighted adjacency matrix
FLUKE <- adjacency.matrix(edges, labels=TRUE)

FLUKE_Drivers <- c("Economic Drivers","Temperature","Shifts in Preferences","Community Vulnerability","Freshwater Influx","Nutrient Influx","Ocean Acidification",
                   "Ocean Features","Oceanographic Transport","Dissolved Oxygen","Water Diversion")
FLUKE_Habitat <- c("Estuarine Habitat","Food Web Changes","Offshore Habitat","Habitat Alteration","Habitat Disturbance","Loose Inert Substrate","Salinity","Water Clarity",
                   "Aquatic Vegetation")
FLUKE_Biota <- c("Fluke Distributional Shift","Fluke Recruitment","Fluke SSB","Adults & Spawners","Age & Size Structure",
                 "Growth","Maturation","Natural Mortality","Sex Ratio")
FLUKE_Species <- c("Other Species Ditributional Shifts","Protected Species")
FLUKE_Management <- c("Allocation","Communication","Enforcement","Management Control","Other Regulations",
                      "Permit Access","Regulatory Complexity","Risk Buffering")
FLUKE_Benefits <- c("Commercial Profits","Consumer Surplus","Recreational Value","Seafood","Recreational Profits")
FLUKE_Science <- c("Assessment Process","Data Quality","Predictability of Recreational Fishing","Stock Assessment")
FLUKE_Fishery <- c("Compliance","Discards","Fishery Distributional Shift","Fishery Resilience",
                   "Fleet Diversity","Landings","Perceived Inequity","Technical Interactions")

FLUKE_C <- brewer.pal(8,"Dark2")

FLUKE_Colors <- data.frame(row.names(FLUKE))
colnames(FLUKE_Colors) <- "Focus"

FLUKE_Colors$Color <- FLUKE_C[1]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Habitat] <- FLUKE_C[2]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Biota] <- FLUKE_C[3]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Species] <- FLUKE_C[4]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Management] <- FLUKE_C[5]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Benefits] <- FLUKE_C[6]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Science] <- FLUKE_C[7]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Fishery] <- FLUKE_C[8]

  FLUKE_Groups <- FLUKE_Colors
    FLUKE_Groups$Group <- "Drivers"
    FLUKE_Groups$Rank <- 1
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Habitat] <- "Habitat"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Habitat] <- 2
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Biota] <- "Fluke Dynamics"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Biota] <- 3
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Species] <- "Other Biota"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Species] <- 4
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Management] <- "Management"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Management] <- 5
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Benefits] <- "Benefits"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Benefits] <- 6
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Science] <- "Science"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Science] <- 7
    FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Fishery] <- "Fishing Fleets"
    FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Fishery] <- 8

  FLUKE_edges <- cbind(FLUKE,FLUKE_Colors)
  FLUKE_Colors <- FLUKE_Colors[order(FLUKE_Colors$Color,FLUKE_Colors$Focus),]
  FLUKE_Colors <- matrix(FLUKE_Colors$Color,dimnames=list(FLUKE_Colors$Focus,"Color"))
  FLUKE_edges <-  FLUKE_edges[order( FLUKE_edges$Color,FLUKE_edges$Focus),]

  FLUKE_edges$Color <- NULL
  FLUKE_edges$Focus <- NULL


 FLUKE_edges <- data.matrix(FLUKE_edges)
  Border_mat <- matrix(1,nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
    rownames(Border_mat) <- rownames(FLUKE_edges)
    colnames(Border_mat) <- colnames(FLUKE_edges)
    #Border_mat[Grand_Banks_edges < 0] = 2
  Border_Col <- matrix("white",nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
  rownames(Border_Col) <- rownames(FLUKE_edges)
  colnames(Border_Col) <- colnames(FLUKE_edges)
  #Border_Col[Grand_Banks_edges < 0] = "black"

  Border_w <- matrix(.0001,nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
  rownames(Border_w) <- rownames(FLUKE_edges)
  colnames(Border_w) <- colnames(FLUKE_edges)

##Full model figure
pdf("X:/gdepiper/MAFMC EAFM/Summer_Flounder/FULL_Circular_v1.pdf",width = 6.69291, height=6.69291)
 print(chordDiagram(FLUKE_edges, directional=0,
                 grid.col = FLUKE_Colors,
                 row.col = FLUKE_Colors,
                 link.lty = Border_mat,
                 link.lwd = Border_w,
                 link.border = Border_Col,
                 annotationTrack="grid",preAllocateTracks= list(track.height=0.5)))

    print(circos.trackPlotRegion(track.index=1, panel.fun= function (x,y){
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim),ylim[1],sector.name,facing="clockwise", niceFacing=TRUE, adj =c(0,0.5), cex=.6)
      }, bg.border=NA) )

    print(legend(x=-1.1,y = 1.09,legend = c("Driver","Habitat","Fluke","Other Biota","Management","Benefits","Science","Fishing Fleets"),
                 lty= c(1,1,1,1,1,1,1,1), lwd=c(5,5,5,5,5,5,5,5),
           col =c(FLUKE_C[1],FLUKE_C[2],FLUKE_C[3],FLUKE_C[4],FLUKE_C[5],FLUKE_C[6],FLUKE_C[7],FLUKE_C[8]), ncol=1, cex = .75, bg = NULL, box.col=NULL, bty = "n"))
    print(title(main="Fluke System", line=-35))

dev.off()

 #########Submodels
Submodel_edges <- function (y) {
  New_edges <- FLUKE_edges[,colnames(FLUKE_edges)%in%y]
  New_edges <- New_edges[rownames(New_edges)%in%y,]
    return(New_edges)
  }
Submodel_color <- function (y) {
  New_colors <-  FLUKE_Colors[rownames(FLUKE_Colors)%in%y,]
  return(New_colors)
}
Submodel_bordermat <- function (y) {
  New_bordermat <- Border_mat[rownames(Border_mat)%in%y,]
  New_bordermat <- New_bordermat[rownames(New_bordermat)%in%y,]
  return(New_bordermat)
}
Submodel_w <- function (y,New_bordermat) {
  New_w <- Border_w[rownames(Border_mat)%in%y,]
  New_w <- New_bordermat[rownames(New_bordermat)%in%y,]
  return(New_w)
}

New_Figure <- function (x) {

New_edges <- Submodel_edges(x)
New_colors <- Submodel_color(x)
New_bordermat <- Submodel_bordermat(x)
New_w <- Submodel_w(x,New_bordermat)
New_groups <- FLUKE_Groups[which(FLUKE_Groups$Focus%in%x),]
  New_groups <- unique(New_groups[,2:4])
  New_groups <- New_groups[order(New_groups$Rank),]
  New_groups <- New_groups[,1:2]
  Figure_name <- deparse(substitute(x))
  Figure_title <- gsub("_"," ",Figure_name)

 pdf(paste0("X:/gdepiper/MAFMC EAFM/Summer_Flounder/",Figure_name,"_v1.pdf",sep=""),width = 6.69291, height=6.69291)
 print(chordDiagram(New_edges, directional=0,
                    grid.col = New_colors,
                    row.col = New_colors,
                    link.lty = New_bordermat,
                    link.lwd = New_w,
                    link.border = "white",
                    annotationTrack="grid",preAllocateTracks= list(track.height=0.5)))

 print(circos.trackPlotRegion(track.index=1, panel.fun= function (x,y){
   xlim = get.cell.meta.data("xlim")
   ylim = get.cell.meta.data("ylim")
   sector.name = get.cell.meta.data("sector.index")
   circos.text(mean(xlim),ylim[1],sector.name,facing="clockwise", niceFacing=TRUE, adj =c(0,0.5), cex=.6)
 }, bg.border=NA) )

 print(legend(x=-1.1,y = 1.09,legend = New_groups$Group,
              lty= rep(1,nrow(New_groups)), lwd=rep(5,nrow(New_groups)),
              col =New_groups$Color, ncol=1, cex = .75, bg = NULL, box.col=NULL, bty = "n"))
 print(title(main=paste("Fluke",Figure_title,sep=" "), line=-30))
 dev.off()
}

#1. Risk Factors
Risk_Factors <- c('Allocation','Commercial Profits','Discards','Fishery Resilience','Fleet Diversity','Management Control',
                  'Recreational Value','Regulatory Complexity','Seafood','Technical Interactions',"Fluke Distributional Shift",
                  "Estuarine habitat", "Fluke SSB","Stock Assessment","Offshore habitat")
New_Figure(Risk_Factors)

#2. Distributional Change
Distributional_Change <- c("Fluke Distributional Shift","Temperature","Fluke Recruitment","Discards",
                           'Landings',"Food Web Changes","Fluke SSB","Estuarine habitat","Offshore habitat")
New_Figure(Distributional_Change)

#3. Estuarine Habitat
Estuarine_Habitat <- c("Estuarine Habitat","Temperature","Dissolved Oxygen","Habitat Alteration","Food Web Changes","Salinity","Freshwater Influx","Loose Inert Substrate",
                       "Water Clarity","Aquatic Vegetation","Nutrient Influx","Water Diversion")
New_Figure(Estuarine_Habitat)

#4. Stock Biomass
Stock_Biomass <- c("Fluke SSB","Estuarine Habitat","Offshore habitat","Food Web Changes","Growth","Maturation","Natural Mortality",
                   'Landings',"Discards","Sex Ratio","Age & Size Structure","Fluke Recruitment","Adults & Spawners","Fluke Distributional Shift")
New_Figure(Stock_Biomass)

#5. Stock Assessment
Stock_Assessment <- c("Stock Assessment","Assessment Process","Data Quality","Risk Buffering")
New_Figure(Stock_Assessment)

#6. Offshore Habitat
Offshore_Habitat <- c("Offshore Habitat","Temperature","Dissolved Oxygen","Salinity","Ocean Acidification","Ocean Features","Food Web Changes",
                      "Habitat Alteration","Habitat Disturbance")
New_Figure(Offshore_Habitat)

#7. Allocation
Allocation <- c("Allocation","Fluke Distributional Shift","Fishery Distributional Shift","Data Quality")
New_Figure(Allocation)

#8. Commercial Profits
Commercial_Profits <- c("Commercial Profits","Compliance",'Fishery Resilience',"Fluke Distributional Shift","Landings","Allocation","Fluke SSB",
                "Management Control","Regulatory Complexity","Economic Drivers","Technical Interactions")
New_Figure(Commercial_Profits)

#9. Discards
Discards <- c("Discards","Fluke Distributional Shift","Allocation","Management Control","Fluke SSB","Fluke Recruitment","Technical Interactions")
New_Figure(Discards)

#10. Fishery Resilience
Fishery_Resilience <- c("Fishery Resilience","Allocation",'Landings',"Permit Access","Fluke Distributional Shift")
New_Figure(Fishery_Resilience)

#11. Fleet Diversity
Fleet_Diversity <- c("Fleet Diversity","Fishery Resilience","Fluke Distributional Shift","Allocation","Permit Access")
New_Figure(Fleet_Diversity)

#12. Management Control
Management_Control <- c("Management Control","Stock Assessment","Enforcement","Compliance","Risk Buffering","Data Quality",
                        "Predictability of Recreational Fishing","Technical Interactions")
New_Figure(Management_Control)

#13. Recreational Value
Recreational_Value <- c("Management Control","Recreational_Value","Fluke Distributional Shift","Allocation","Consumer Surplus",
                        "Shifts in Preferences","Economic Drivers","Regulatory Complexity","Landings","Fluke SSB","Recreational Profits","Other Species Ditributional Shifts")
New_Figure(Recreational_Value)

#14. Regulatory Complexity
Regulatory_Complexity <- c("Regulatory Complexity","Perceived Inequity","Allocation","Communication","Data Quality","Management Control")
New_Figure(Regulatory_Complexity)

#15. Seafood Production
Seafood_Production <- c("Seafood","Discards","Consumer Surplus","Landings","Economic Drivers","Fishery Resilience")
New_Figure(Seafood_Production)

#16. Technical Interactions
Technical_Interactions <- c("Technical Interactions","Protected Species","Fluke Distributional Shift",
                            "Other Species Ditributional Shifts","Communication","Regulatory Complexity")
New_Figure(Technical_Interactions)
