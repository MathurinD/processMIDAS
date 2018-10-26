#################### processControls ##########################
# Functions related to the definition of controls in the data and normalisation

#' @import STASNet
verbose = 2

#' Normalise by a control condition
#'
#' Normalise all values by the respective control and renames the ID:type column of a midas file corresponding to control treatment
#' @param midas_file A MIDAS matrix or filename
#' @param control_condition A string depicting the control condition. Each control treatment has to be separated by a '+'.
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @examples normaliseByControls("blunt_MIDAS.csv", "BSA+DMSO") # BSA and DMSO are the controls used for the inhibitors and ligands respectively
#' @export
normaliseByControls <- function(midas_file, control_condition, save_file=FALSE) {
# TODO: if several MIDAS files are provided, normalise by the mean of all the common treatments (more robust normalisation method) (and merge them?)

    controls = unlist(strsplit(control_condition, "\\+"))
    output_names = c()

    midas_file = extractMIDAS(midas_file)
    if (length(controls) == 1 && controls=="") {
        # special case where the control corresponds to no treatment
        control_lines = sapply(1:nrow(midas_file), function(rr) { all(midas_file[rr,grepl("^TR.", colnames(midas_file))]==0) })
    } else {
        scols = which(colnames(midas_file) %in% paste0("TR.", controls))
        control_lines = sapply(1:nrow(midas_file), function(rr) { all(midas_file[rr,scols]==1) })
        save_midas = midas_file[,-scols]
    }
    save_midas$ID.type = as.character(save_midas$ID.type)
    save_midas[control_lines, "ID.type"] = "control"
    dv_cols = colnames(save_midas)[grepl("^DV.", colnames(save_midas))]
    mean_controls = colMeans(save_midas[control_lines, dv_cols], na.rm=TRUE)
    save_midas[control_lines, dv_cols] = sapply(dv_cols, function(xx) { save_midas[control_lines,xx]/mean_controls[xx] })
    save_midas[!control_lines, dv_cols] = sapply(dv_cols, function(xx) { save_midas[!control_lines,xx]/mean_controls[xx] })

    if (save_file) {
        save_name = paste0(dirname(midas_file), "/", control_condition, "_normalised_", basename(midas_file))
        save_name = gsub("blunt_", "", save_name) # The file won't be blunt anymore
        write.csv(save_midas, file=save_name, quote=FALSE, row.names=FALSE)
    }
    return(save_midas)
}


#' Identify the controls in a MIDAS file
#'
#' Rename the ID:type column of a MIDAS file which correspond to control treatment
#' @param midas_file A MIDAS matrix or filename, or a vector of filenames
#' @param control_condition A string depicting the control condition. Each control treatment has to be separated by a '+'.
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @examples defineControls("blunt_MIDAS.csv", "BSA+DMSO")
#' @export
defineControls <- function(midas_file, control_condition, save_file=FALSE) {
    pipeline = FALSE
    controls = unlist(strsplit(control_condition, "\\+"))

    midas_file = extractMIDAS(midas_file)
    scols = which(colnames(midas_file) %in% paste0("TR.", controls))
    if (length(scols) == 0) { stop(paste0("None of the specified controls (", paste0(controls, collapse=", "), ") have been found")) }
    control_lines = sapply(1:nrow(midas_file), function(rr) { all(midas_file[rr,scols]==1) })
    save_midas = midas_file[,-scols]
    save_midas[, "ID.type"] = as.character(save_midas[, "ID.type"]) # Because extractMIDAS extracts as factors
    save_midas[control_lines, "ID.type"] = "control"

    #save_name = paste0(dirname(midas_file), "/", control_condition, "_control_", basename(midas_file))
    save_name = "control"
    if (save_file) {
        write.csv(save_midas, file=save_name, quote=FALSE, row.names=FALSE)
    }
    return(save_midas)
}
