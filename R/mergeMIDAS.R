#' Merge several MIDAS together
#'
#' Merge MIDAS files together, adding NA to the fields that are not shared
#' @param ... MIDAS filenames or matrices
#' @param save_file Whether the resulting MIDAS matrix should be saved in a file
#' @param save_name Name to give to the merged file
#' @examples mergeMIDAS("blunt1_MIDAS.csv", "blunt2_MIDAS.csv")
#' @export
#' @seealso extractMIDAS
mergeMIDAS <- function(..., save_file=FALSE, save_name="default") {
    mfiles = list(...)
    all_ids = c()
    all_measurements = c()
    all_treatments = c()
    midas_list = list()
    # Collect all column names that should be in the merged file
    fid = 0
    for (ff in mfiles) {
        fid = fid+1
        midas_list[[fid]] = extractMIDAS(ff)
        midas = midas_list[[fid]]
        all_ids = unique(c(all_ids, colnames(midas)[grepl("^ID.", colnames(midas))]))
        all_treatments = unique(c(all_treatments, colnames(midas)[grepl("^TR.", colnames(midas))]))
        all_measurements = unique(c(all_measurements, colnames(midas)[grepl("^DV.", colnames(midas))]))
    }
    # Generate mergeable version of each MIDAS file
    for (fid in 1:length(mfiles)) {
        midas = midas_list[[fid]]
        zero_col = rep(0, nrow(midas))
        na_col = rep(NA, nrow(midas))
        output = data.frame(row.names=1:nrow(midas))
        for (id in all_ids) {
            if (id %in% colnames(midas)) {
                output = cbind(output, midas[,id])
            } else {
                output = cbind(output, na_col)
            }
        }
        for (tt in all_treatments) {
            if (tt %in% colnames(midas)) {
                output = cbind(output, midas[,tt])
            } else {
                output = cbind(output, zero_col)
            }
        }
        for (mm in all_measurements) {
            if (mm %in% colnames(midas)) {
                output = cbind(output, midas[,mm])
            } else {
                output = cbind(output, na_col)
            }
        }
        colnames(output) = c(all_ids, all_treatments, all_measurements)
        midas_list[[fid]] = output
    }
    save_midas = midas_list[[1]]
    for (midas in midas_list[-1]) {
        save_midas = rbind(save_midas, midas)
    }

    save_dir = "merged" # TODO filter here too
    dir.create(save_dir, FALSE)
    tryCatch({
        save_file = paste0(save_dir, "/", "merged_", basename(mfiles[1])) # TODO extra filter for commun names
        }, error=function(X){
            save_file = paste0(save_dir, "/", save_name, "MIDAS.csv")
        })
    if (save_file) {
        write.csv(save_midas, file=save_file, quote=FALSE, row.names=FALSE)
    }
    return(save_midas)
}

