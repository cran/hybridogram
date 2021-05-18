#' @name hybridogram
#' @title Function that Creates a Heat Map from Hybridization Data
#' @description R package which takes a list of hybridization results along
#' with a code and creates a heat map.
#'
#' Version 2.3
#' Author: Dr. Matthew Cserhati
#' Email: csmatyi@protonmail.com
#' May 15, 2021
#'
#' @importFrom grDevices colorRampPalette dev.off jpeg
#' @importFrom graphics box legend
#'
#' @param hybrid_data a data frame with three columns: species1, species2, code
#' @param codes a data frame with two columns: code, description
#' @return nil
#'
#' @references
#' Wood, T. C., and Murray, M. J. (2003) Understanding the Pattern of Life. Nashville, TN: Broadman & Holman.
#'
#' @examples
#' V1 <- c("Phoca largha","Phoca largha","Phoca caspica")
#' V2 <- c("Phoca vitulina","Phoca caspica","Pusa hispida")
#' V3 <- c(2,3,3)
#' hybrid_data <- data.frame(V1,V2,V3)
#' C1 <- c(1,2,3)
#' C2 <- c("No hybrid","Documented hybrid","Hybrid with same 3rd species")
#' codes <- data.frame(C1,C2)
#' hybridogram(hybrid_data, codes)
#'
#' @export
utils::globalVariables(c("exit","heatmap.2"))
hybridogram <- function(hybrid_data, codes) {
  if ((dim(hybrid_data)[1] < 2) | (dim(hybrid_data)[2] != 3)) {
    print("Improper format of hybridization file!")
    exit()
  }

  # give code data frame column names
  colnames(hybrid_data) <- c("species1","species2","code")

  # Read in code data
  # number, code number meaning
  if ((dim(codes)[1] < 2) | (dim(codes)[2] != 2)) {
    print("Improper format of hybridization file!")
    exit()
  }

  # give codes data frame column names
  colnames(codes) <- c("code","description")

  n_codes <- dim(codes)[1]
  descrs <- codes$description
  n_codes <- n_codes + 1

  codes[n_codes,]$code <- n_codes
  codes[n_codes,]$description <- "Self"

  # Color code
  colors <- colorRampPalette(c("white","yellow","red"))(n_codes)
  descrs_colors <- structure(names=descrs,colors)
  descrs_colors_vector <- as.vector(descrs_colors)

  # get species list
  species <- sort(unique(c(hybrid_data$species1,hybrid_data$species2)))
  n_species <- length(species)

  # Create H hybridogram matrix
  H <- matrix(0L,nrow=n_species,ncol=n_species)
  colnames(H) <- species
  rownames(H) <- species
  for (i in 1:dim(hybrid_data)[1]) {
    s1 <- hybrid_data[i,1]
    s2 <- hybrid_data[i,2]
    code <- hybrid_data[i,3]
    H[s1,s2] = code
    H[s2,s1] = code
    H[s1,s1] = n_codes
    H[s2,s2] = n_codes
  }

  clrplt <- descrs_colors_vector
  cexx <- 2
  ceyy <- cexx

  h <- gplots::heatmap.2(H,dendrogram="none",Rowv=NA,Colv=NA,col=clrplt,margin=c(25,25),trace="none",
            symkey=F,key=F,scale="none",cexRow=cexx,cexCol=ceyy,sepcolor="both",
            labCol=as.expression(lapply(colnames(H), function(a) bquote(italic(.(a))))),
            labRow=as.expression(lapply(rownames(H), function(a) bquote(italic(.(a))))))

  legend(x="bottomright",legend=codes$description,cex=1,fill=clrplt)
  box(which="plot",lty="solid")
}
