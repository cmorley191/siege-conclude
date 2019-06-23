namespace SiegeConclude

open System.Drawing
open System.Collections.Generic

[<AutoOpen>]
module IESpecModule =
  type ColorCheckLogic =
    | Expect of RGBColor
    | ExpectAny of RGBColor list
    | ExpectNot of RGBColor
    | ExpectNotAny of RGBColor list  

  type ColorCheck =
    {
      Logic: ColorCheckLogic
      Threshold: ColorDistance
    }

    static member check pixel { Logic = logic; Threshold = threshold; } = 
      match logic with
      | Expect color -> (color |-| pixel) <= threshold
      | ExpectAny colors -> colors |> List.exists (fun color -> (color |-| pixel) <= threshold)
      | ExpectNot color -> (color |-| pixel) > threshold
      | ExpectNotAny colors -> colors |> List.forall (fun color -> (color |-| pixel) > threshold)
    member this.Check pixel = this |> ColorCheck.check pixel

  /// <summary>
  /// A sentinel color in an IESpecTemplate, used to visually specify pixel locations that are targeted by a specific ColorCheck.
  /// </summary>
  type Paint = 
    | Paint of RGBColor

    static member value (Paint value) = value
    member this.Value = this |> Paint.value

  module Paints =
    let noCheck = Paint <| RGBColor (0uy, 0uy, 0uy)
    let red = Paint <| RGBColor (255uy, 0uy, 0uy)
    let green = Paint <| RGBColor (0uy, 255uy, 0uy)
    let blue = Paint <| RGBColor (0uy, 0uy, 255uy)
    let yellow = Paint <| RGBColor (255uy, 255uy, 0uy)

    let recognized = 
      (fun () -> Set.ofList [noCheck; red; green; blue; yellow])
      |> Safety.addContext "A pair of recognized paints have the same color value, suggesting they might specify conflicting checks."

  /// <summary>
  /// A template image, and associated data used to represent an IESpec. This is the "human-accessible" version of an IESpec, since it is visual-based; algorithms use the assembled IESpec.
  /// </summary>
  type IESpecTemplate = 
    {
      TemplatePath: ImageFilePath
      Offset: Position
      PaintChecks: (Paint * ColorCheck) list 
    }

  type private StartedPaint = StartedPaint of minX:int * paint:(Paint * ColorCheck)
  type private TemplateRowParseState = 
    | NoCurrentPaint of (Row * ColorCheck) list
    | CurrentPaint of StartedPaint * (Row * ColorCheck) list

  /// <summary>
  /// An Interface Element Specification. Contains the information used to identify a specific element of the Siege interface in a screenshot.
  /// </summary>
  type IESpec =
    {
      Checks: (Row * ColorCheck) list
    }

    static member load ({ TemplatePath = templatePath; Offset = templateOffset; PaintChecks = templateChecks } as template) =
      do 
        (fun () -> 
          templateChecks
          |> Seq.map fst
          |> Set.ofSeq
          |> ignore
        ) |> Safety.addContext (sprintf "A pair of paints specified in the check for '%s' have the same color value, suggesting they might specify conflicting checks." (string template))

      use templateImage = templatePath |> ImageFilePath.openImage

      let checks = 
        templateImage
        |> ImageUtil.allPixelsByRow
        |> Seq.indexed
        |> Seq.collect (fun (y, rowPixels) ->
          let finalizePaint (StartedPaint (minX, (_, currentCheck))) maxX checks =
            let paintRow = Row (IntRange (templateOffset.X + minX, templateOffset.X + maxX), templateOffset.Y + y)
            (paintRow, currentCheck) :: checks

          let rowChecksFoldResult =
            rowPixels
            |> Seq.indexed
            |> Seq.fold 
              (fun state (x, pixel) ->
                let checkForNewPaint checks =
                  if pixel = Paints.noCheck.Value
                  then NoCurrentPaint checks
                  else 
                    let current = 
                      (fun () -> templateChecks |> List.find (fun (paint, _) -> paint.Value = pixel))
                      |> Safety.addContextWhen is<KeyNotFoundException> (sprintf "Template image for '%s' contains an unrecognized paint color: %s" (string template) (string pixel))
                    CurrentPaint (StartedPaint (x, current), checks)

                match state with
                | NoCurrentPaint checks -> checkForNewPaint checks
                | CurrentPaint (StartedPaint (_, (currentPaint, _)) as startedPaint, checks) as currentPaintState ->
                  if pixel = currentPaint.Value 
                  then currentPaintState
                  else
                    let checks = finalizePaint startedPaint (x - 1) checks
                    checkForNewPaint checks
              )
              (NoCurrentPaint [])

          match rowChecksFoldResult with
          | NoCurrentPaint checks -> checks
          | CurrentPaint (startedPaint, checks) -> finalizePaint startedPaint (rowPixels.Length - 1) checks
        )
        |> Seq.toList
        
      printfn "IESpec: Loaded template at '%s'" templatePath.Path
      {
        Checks = checks
      }