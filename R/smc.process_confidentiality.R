#' @title to aggregate confidential (秘匿) mesh values into their destination mesh
#' @description \code{smc.process_confidentiality}
#' @details
#' 国勢調査のメッシュ統計では、対象数が少ないメッシュは「秘匿メッシュ」として
#' 数値が伏せられ（テキスト上は \code{*}）、その数値は隣接する
#' 「合算先メッシュ」に加算された状態で公表される。
#'
#' ただし全項目が合算されるわけではない。実データを確認すると、
#' 「人口（総数・男・女）」「世帯総数」「一般世帯数」など一部の項目は
#' 秘匿メッシュ側に実数が残っており、合算先には加算されていない。
#' そのため合算先メッシュでは
#' 例えば「人口（総数）＜ ０〜１４歳＋１５歳以上」のような不整合が生じる。
#'
#' この関数は、秘匿メッシュ（\code{HTKSYORI} が 2、\code{HTKSAKI} に合算先の
#' KEY_CODE が入る行）に残っている実数を合算先メッシュへ加算し、
#' 秘匿メッシュ側の値を NA にする。これにより
#' \itemize{
#'   \item 合算先メッシュの各項目が同じ母数で整合する（比率計算が正しくなる）
#'   \item 全メッシュを合計しても二重計上にならない
#' }
#'
#' 判定はデータそのものから行う。ある統計表について秘匿メッシュの値に
#' ひとつでも \code{*}（= NA）があれば「その表では秘匿されている」とみなし、
#' 残っている実数を合算先に加算する。すべて実数であればその表では
#' 秘匿されていないと判断して何もしない。
#' このため測地系（JGD2000 / JGD2011）で秘匿の対象がわずかに異なる場合でも
#' 誤って加算されることはない。
#'
#' 統計値の列（\code{T} + 9桁）はすべて numeric に変換されて返る。
#'
#' @param mesh smc.read_census_mesh_2020* / smc.get_census_mesh_2020* の出力
#'   （data.frame でも sf でもよい）
#' @param hitoku_value 加算後に秘匿メッシュ側の値をどうするか。
#'   \code{"na"}（既定）= NA にする、\code{"zero"} = 0 にする、
#'   \code{"keep"} = そのまま残す（合計が二重計上になるので注意）
#' @param saki_col 合算先メッシュのKEY_CODEが入っている列名。
#'   NULL（既定）なら \code{HTKSAKI} / \code{HTKSAKI.x} などを自動で探す
#' @param quiet TRUE のとき処理件数のメッセージを出さない
#' @return 秘匿処理後のデータ（入力と同じクラス）
#' @export

smc.process_confidentiality <- function(mesh,
                                        hitoku_value = c("na", "zero", "keep"),
                                        saki_col = NULL,
                                        quiet = FALSE){
  hitoku_value <- match.arg(hitoku_value)

  nm <- names(mesh)

  # ---------- 列の特定 ---------- #
  # 統計値の列は "T" + 9桁（例: T001102001）
  value_cols <- grep("^T[0-9]{9}$", nm, value = TRUE)
  if (length(value_cols) == 0) {
    stop("統計値の列（T + 9桁）が見つかりません。smc.read_census_mesh_2020* の出力を渡してください。")
  }
  if (!("KEY_CODE" %in% nm)) {
    stop("KEY_CODE 列が見つかりません。")
  }
  if (is.null(saki_col)) {
    # 結合の仕方によって HTKSAKI / HTKSAKI.x などになりうる
    cand <- unique(c("HTKSAKI", "HTKSAKI.x", grep("^HTKSAKI", nm, value = TRUE)))
    cand <- cand[cand %in% nm]
    if (length(cand) == 0) {
      stop("HTKSAKI 列が見つかりません。saki_col で列名を指定してください。")
    }
    saki_col <- cand[1]
  }
  if (!(saki_col %in% nm)) {
    stop(paste0("列が見つかりません: ", saki_col))
  }

  # ---------- キーを文字列に正規化 ---------- #
  as_key <- function(x){
    if (is.numeric(x)) {
      x <- formatC(x, format = "d")
    }
    x <- trimws(as.character(x))
    x[x == "" | x == "NA" | x == "-"] <- NA_character_
    x
  }
  key  <- as_key(mesh[["KEY_CODE"]])
  saki <- as_key(mesh[[saki_col]])

  # ---------- 統計値を数値行列に ---------- #
  # "*"（秘匿）は NA になる
  mat <- matrix(NA_real_,
                nrow = length(key), ncol = length(value_cols),
                dimnames = list(NULL, value_cols))
  for (k in seq_along(value_cols)) {
    mat[, k] <- suppressWarnings(as.numeric(mesh[[value_cols[k]]]))
  }

  # ---------- 秘匿メッシュと合算先の対応 ---------- #
  src_all  <- which(!is.na(saki))
  if (length(src_all) == 0) {
    if (!quiet) message("秘匿メッシュ（HTKSAKI が入っている行）はありませんでした。")
    for (k in seq_along(value_cols)) mesh[[value_cols[k]]] <- mat[, k]
    return(mesh)
  }
  dest_pos <- match(saki[src_all], key)
  n_missing <- sum(is.na(dest_pos))

  # ---------- 統計表ごとに加算 ---------- #
  tbl_id  <- substr(value_cols, 1, 7)   # 例: T001102
  touched <- rep(FALSE, length(src_all))

  for (g in unique(tbl_id)) {
    cols <- which(tbl_id == g)
    sub  <- mat[src_all, cols, drop = FALSE]
    # その統計表で実際に秘匿されている行だけを対象にする
    is_hitoku <- rowSums(is.na(sub)) > 0
    use <- is_hitoku & !is.na(dest_pos)
    if (!any(use)) next

    rows <- src_all[use]
    tgt  <- dest_pos[use]

    add_mat <- mat[rows, cols, drop = FALSE]
    add_mat[is.na(add_mat)] <- 0
    agg <- rowsum(add_mat, group = tgt, reorder = FALSE)
    tgt_rows <- as.integer(rownames(agg))

    mat[tgt_rows, cols] <- mat[tgt_rows, cols, drop = FALSE] + agg

    if (hitoku_value == "na")   mat[rows, cols] <- NA_real_
    if (hitoku_value == "zero") mat[rows, cols] <- 0

    touched <- touched | use
  }

  # ---------- 書き戻し ---------- #
  for (k in seq_along(value_cols)) {
    mesh[[value_cols[k]]] <- mat[, k]
  }

  if (!quiet) {
    message(sprintf(
      "秘匿メッシュ %d 件のうち %d 件を合算先 %d 件へ加算しました（統計表 %d 表、合算先列: %s）。",
      length(src_all), sum(touched),
      length(unique(dest_pos[!is.na(dest_pos)])),
      length(unique(tbl_id)), saki_col
    ))
    if (n_missing > 0) {
      message(sprintf(
        "うち %d 件は合算先メッシュがデータ内に無いためスキップしました。",
        n_missing
      ))
    }
  }

  return(mesh)
}


#' @title deprecated: use smc.process_confidentiality
#' @description \code{smc.process_cenfidentiality}
#' @param mesh smc.read_census_mesh_2020* / smc.get_census_mesh_2020* の出力
#' @param ... smc.process_confidentiality に渡す引数
#' @return 秘匿処理後のデータ
#' @export

smc.process_cenfidentiality <- function(mesh, ...){
  .Deprecated("smc.process_confidentiality")
  smc.process_confidentiality(mesh, ...)
}
