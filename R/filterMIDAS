# coding * utf-8 *

#' Filter out entries from a MIDAS
#'
#' Filter out some entries from a MIDAS file by setting them to NA
#' @param midas_file A MIDAS matrix or filename
#' @param filters A list of filters. A filter is of the form READOUT, PERT which means that the entries for PERT or READOUT are removed, or of the form READOUT@PERT, in which case the entries for READOUT when perturbation PERT is present are removed
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @example filterMIDAS blunt_MIDAS.csv AKT@AKTi
#' @export
#' @seealso extractMIDAS, defineControls
#' NOTE: this script assumes that perturbation and readout names are distinct
filterMIDAS <- function(midas_file, filters, save_file=FALSE) {
    midas_file = extractMIDAS(midas_file)

    remove_cols = c()
    remove_rows = c()
    pert_names = gsub("^[A-Z]{2}.", "", colnames(midas_file)[grepl("TR", colnames(midas_file))])
    start_pert = min(which(grepl("TR", colnames(midas_file))))-1
    readout_names = gsub("^[A-Z]{2}.", "", colnames(midas_file)[grepl("DV", colnames(midas_file))])
    start_readout = min(which(grepl("DV", colnames(midas_file))))-1

    for (filter in filters) {
        if (filter %in% readout_names) {
            remove_cols = c(remove_cols, start_readout+which(readout_names == filter))
        } else if (filter %in% pert_names) {
            selected = start_pert+which(pert_names == filter)
            remove_cols = c(remove_cols, selected)
            remove_rows = unique(c( which(midas_file[,selected] == 1), remove_rows ))
        } else if (grepl("@", filter)) {
            both = unlist(strsplit(filter, "@"))
            readout = both[1]
            pert = both[2]
            if (readout %in% readout_names && pert %in% pert_names) {
                scol = start_pert+which(pert_names == pert) # Perturbation column
                srows = which(midas_file[,scol] == 1) # Rows with the perturbation
                readout_col = start_readout+which(readout_names == readout) # Readout column
                midas_file[srows, readout_col] = NA
            } else {
                message(paste("Could not find the combination '", filter, "'"))
            }
        } else {
            message(paste("'", filter, "' could not be identified as a perturbation or readout"))
        }
    }
    if (length(remove_cols) > 1) {
        if (length(remove_rows) > 1) {
            save_midas = midas_file[-remove_rows,-remove_cols]
        } else {
            save_midas = midas_file[,-remove_cols]
        }
    } else if (length(remove_rows) > 1) {
            save_midas = midas_file[-remove_rows,]
    } else {
        save_midas = midas_file
    }
    output_name = gsub("MIDAS", paste0("filter_", paste0(cargs[-1], collapse="-"), "_MIDAS"), cargs[1])
    output_name = gsub("blunt_", "", output_name) # The file won't be blunt anymore
    if (save_file) {
        write.csv(save_midas, file="", quote=FALSE, row.names=FALSE)
    }
    return(save_midas)
}


