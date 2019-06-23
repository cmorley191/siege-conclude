module SiegeConclude.InterfaceAnalyzer

open System.Drawing

let identifyOffsetElement (Position (dx, dy) as _offset) ({ Checks = checks } as _elementSpec) (screenshot:Bitmap) =
  checks |> Seq.indexed |> Seq.forall (fun (iCheck, (Row (xs, y), check)) -> 
    let xs = xs + dx
    let y = y + dy

    xs.Seq |> Seq.forall (fun x ->
      let pixel = screenshot.GetPixel(x, y) |> RGBColor.ofColor
      let checkResult = check.Check pixel
      checkResult
    )
  )

let identifyElement elementSpec screenshot = let offset = (Position (0, 0)) in identifyOffsetElement offset elementSpec screenshot

