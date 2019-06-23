module SiegeConclude.Safety

open System

let contextExceptionDataKey = "Added Context"
let private addContextToException context (ex:exn) = 
  if ex.Data.Contains(contextExceptionDataKey)
  then ex.Data.[contextExceptionDataKey] <- context :: (ex.Data.[contextExceptionDataKey] :?> string list)
  else ex.Data.[contextExceptionDataKey] <- [context]

let addContext context thunk = 
  try thunk ()
  with ex ->
    ex |> addContextToException context
    reraise ()

let addContextWhen predicate context thunk =
  try thunk ()
  with ex when predicate ex ->
    ex |> addContextToException context
    reraise ()