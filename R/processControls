#' @import STASNet

#' normaliseByControls normalises all values by the respective control and renames the ID:type column of a midas file corresponding to control treatment
#' @param midas_file A MIDAS matrix or filename, or a vector of filenames
#' @param control_condition A string depicting the control condition. Each control treatment has to be separated by a '+'.
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @example normaliseByControls("blunt_MIDAS.csv", "BSA+DMSO") # BSA and DMSO are the controls used for the inhibitors and ligands respectively
#' @export
normaliseByControls <- function(midas_file, control_condition, save_file=FALSE) {
# TODO: if several MIDAS files are provided, normalise by the mean of all the common treatments (more robust normalisation method) (and merge them?)

    controls = unlist(strsplit(control_condition, "\\+"))
    output_names = c()
    library(STASNet)
    for (cc in midas_file) {
        midas_file = extractMIDAS(cc)
        scols = which(colnames(midas_file) %in% paste0("TR.", controls))
        control_lines = sapply(1:nrow(midas_file), function(rr) { all(midas_file[rr,scols]==1) })
        save_midas = midas_file[,-scols]
        save_midas$ID.type = as.character(save_midas$ID.type)
        save_midas[control_lines, "ID.type"] = "control"
        dv_cols = colnames(save_midas)[grepl("^DV.", colnames(save_midas))]
        mean_controls = colMeans(save_midas[control_lines, dv_cols])
        save_midas[control_lines, dv_cols] = sapply(dv_cols, function(xx) { save_midas[control_lines,xx]/mean_controls[xx] })
        save_midas[!control_lines, dv_cols] = sapply(dv_cols, function(xx) { save_midas[!control_lines,xx]/mean_controls[xx] })

        save_name = paste0(dirname(cc), "/", control_condition, "_normalised_", basename(cc))
        save_name = gsub("blunt_", "", save_name) # The file won't be blunt anymore
        output_names = c(output_names, save_name)
        if (save_file) {
            write.csv(save_midas, file="", quote=FALSE, row.names=FALSE)
        }
    }
    return(save_midas)
}


#' Identify the controls in a MIDAS file
#'
#' Rename the ID:type column of a MIDAS file which correspond to control treatment
#' @param midas_file A MIDAS matrix or filename, or a vector of filenames
#' @param control_condition A string depicting the control condition. Each control treatment has to be separated by a '+'.
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @example defineControls("blunt_MIDAS.csv", "BSA+DMSO")
#' @export
defineControls <- function(midas_file, control_condition, save_file=FALSE) {
    pipeline = FALSE
    controls = unlist(strsplit(control_condition, "\\+"))
    output_names = c()
    library(STASNet)
    for (cc in midas_file) {
        midas_file = as.matrix(extractMIDAS(cc))
        scols = which(colnames(midas_file) %in% paste0("TR.", controls))
        control_lines = sapply(1:nrow(midas_file), function(rr) { all(midas_file[rr,scols]==1) })
        save_midas = midas_file[,-scols]
        save_midas[control_lines, "ID.type"] = "control"

        save_name = paste0(dirname(cc), "/", control_condition, "_control_", basename(cc))
        save_name = gsub("blunt_", "", save_name) # The file won't be blunt anymore
        output_names = c(output_names, save_name)
        if (save_file) {
            write.csv(save_midas, file="", quote=FALSE, row.names=FALSE)
        }
    }
    return(save_midas)
}
