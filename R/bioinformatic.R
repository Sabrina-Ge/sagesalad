#' Returns a dataframe of matched HGNC IDs for the inputted Ensembl IDs.
#'
#' @param ensemblIDs vector of character strings representing Ensembl IDs
#' @param version NULL for the newest version, or numerical Ensembl version
#' @param additional vector of additional biomaRt attributes (eg: "transcript_length"). See biomaRt::getBM().
#' 
#' @return dataframe of "ensembl_gene_id", "hgnc_symbol" and any additional attributes
#' 
#' 
#' @export
ensembl.to.hgnc <- function(ensemblIDs, version = NULL, additional = c()){
  requireNamespace("biomaRt")
  if (is.null(version)) {
    mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  } else {
    archive_mart_info <- listEnsemblArchives()
    host <- archive_mart_info$url[archive_mart_info$version == as.character(version)]
    host <- substr(host, 8, nchar(host))
    cat(paste0("Using host: ", host))
    mart <- useMart(host=host, biomart="ENSEMBL_MART_ENSEMBL",
                    dataset="hsapiens_gene_ensembl")
  }

  ids <- getBM(filters = "ensembl_gene_id",
               attributes = c("ensembl_gene_id", "hgnc_symbol", additional),
               values = ensemblIDs, mart = mart);
  return(ids);
}
