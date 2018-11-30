match_parameters <- function(local, global, default) {
  local_parameters <- names(local)
  global_parameters <- setdiff(names(global), local_parameters)
  default_parameters <- setdiff(setdiff(names(default), global_parameters), local_parameters)

  append(local,
         append(global[global_parameters], default[default_parameters])
  )
}
