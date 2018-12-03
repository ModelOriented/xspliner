match_parameters <- function(local, global, default) {
  local_parameters <- names(local)
  global_parameters <- setdiff(names(global), local_parameters)
  default_parameters <- setdiff(setdiff(names(default), global_parameters), local_parameters)

  append(local,
         append(global[global_parameters], default[default_parameters])
  )
}

data_linear_scaling <- function(data, data_to_scale) {
  axis_scaling <- (max(data_to_scale) - min(data_to_scale)) / (max(data) - min(data))
  axis_translation <- max(data_to_scale) - axis_scaling * max(data)
  data_to_scale <- (data_to_scale - axis_translation) / axis_scaling
  list(data = data_to_scale, scaling = axis_scaling, translation = axis_translation)
}
