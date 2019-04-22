
#' Creating Project Directory Structure
#'
#' @return
#' @export
#'
#' @examples
#'
#' start_proj()
#' dir()
start_proj <- function(){
  dir.create('data')
  dir.create('doc')
  dir.create('results')
  dir.create('scripts')
  dir.create('tmp')
}
