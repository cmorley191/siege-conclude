module SiegeConclude.ImageUtil

open System.Drawing

let allPixelsByRow (image:Bitmap) =
  Array.init image.Height (fun y -> 
    Array.init image.Width (fun x -> 
      image.GetPixel(x, y) |> RGBColor.ofColor
    )
  )