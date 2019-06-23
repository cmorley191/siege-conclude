// Learn more about F# at http://fsharp.org

open System
open System.Drawing
open SiegeConclude
open SiegeTypes
open System.Diagnostics

let private timeProcedure message thunk =
  printfn "%s..." message
  let stopwatch = Stopwatch.StartNew()
  let result = thunk()
  stopwatch.Stop()
  printfn "    (%s)" (string stopwatch.Elapsed)
  result


let templateCheckProcedure screenshot =
  let templatesToCheck = [
    SiegeInterfaceElements.OperatorSelectPhase.StatusBar.bar Ally Orange
    SiegeInterfaceElements.OperatorSelectPhase.StatusBar.bar Enemy Blue
  ]

  for template in templatesToCheck do
    let element = 
      timeProcedure 
        (sprintf "Loading IESpec for '%s'" template.TemplatePath.Path)
        (fun () -> template |> IESpec.load)

    let isThere = 
      timeProcedure
        "Identifying"
        (fun () -> screenshot |> InterfaceAnalyzer.identifyElement element)

    if isThere
    then printfn "Yep, it's there."
    else printfn "Nope, not there."

    printf "\n\n"


let modelDetectProcedure screenshots =
  timeProcedure
    "Loading specs"
    SiegeModel.loadModule

  for (path, screenshot) in screenshots do
    let screenshotModel =
      timeProcedure
        (sprintf "Detecting %s" (path |> ImageFilePath.path))
        (fun () -> SiegeModel.detect screenshot)

    printfn "Result:\n%s" (string screenshotModel)

    printf "\n\n"


[<EntryPoint>]
let main argv =
  printfn "Starting SiegeConclude"

  printfn "Loading screenshots..."
  let screenshots = 
    [
      @"C:\Users\snick\Desktop\siegeScreenshot.png"
      @"C:\Users\snick\Desktop\siegeScreenshot2.png"
      @"C:\Users\snick\Desktop\siegeScreenshot3.png"
    ]
    |> List.map ImageFilePath.screenshot
    |> List.map (fun path -> (path, path |> ImageFilePath.openImage))
  
  modelDetectProcedure screenshots

  printfn "\nDone!"
  0 // return an integer exit code
