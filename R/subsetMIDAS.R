#' Create subset of a MIDAS file
#'
#' Create new MIDAS files from a subset of lines of an existing MIDAS file
#' @param midas_file A MIDAS matrix or filename
#' @param subsets A vector of column name from the MIDAS 'TR:' fields. A '!' before the string means that the rows without this field will be included.
#' @param remove_columns Whether the columns corresponding the subsets should be removed in the final MIDAS files
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @param filter_controls Whether the control lines should be filtered. Should be TRUE when filtering for cell lines but FALSE when filtering for conditions.
#' @examples subsetMIDAS("blunt_MIDAS.csv", c("parental", "shp2ko"))
#' @export
subsetMIDAS <- function(midas_file, subsets, remove_columns=TRUE, save_file=FALSE, filter_controls=FALSE) {
    library(STASNet)
    output_names = c()

    midas_file = extractMIDAS(midas_file)
    subsets_list = list()
    for (item in subsets) {
        choice = 1
        if (substr(item, 1, 1) == "!") {
            choice = 0
            item = substr(item, 2, 1000)
        }
        selection = which(colnames(midas_file) == paste0("TR.", item))
        blank_rows = grepl("^(blank)$", midas_file[,"ID.type"])
        control_rows = grepl("^(c|control|norm)$", midas_file[,"ID.type"])
        if (filter_controls) {
            protected_rows = blank_rows
        } else {
            protected_rows = blank_rows | control_rows
        }
        if (remove_columns) {
            save_midas = midas_file[protected_rows | midas_file[,selection]==choice,-selection]
        } else {
            save_midas = midas_file[protected_rows | midas_file[,selection]==choice,]
        }

        save_name = gsub("MIDAS", paste0(ifelse(choice==0, "no-", ""), item, "_MIDAS"), midas_file)
        if (save_file) {
            dir.create(item, showWarnings=FALSE)
            save_name = paste0(item, "/", save_name)
            output_names = c(output_names, save_name)
            write.csv(save_midas, file=save_name, quote=FALSE, row.names=FALSE)
        }
        subsets_list[[item]] = save_midas
    }
    return(subsets_list)
}


