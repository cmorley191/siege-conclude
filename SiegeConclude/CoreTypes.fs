namespace rec SiegeConclude

open System.Drawing

type Dimension =
  | Dimension of w:int * h:int

  static member width (Dimension (w, _)) = w
  member this.Width = this |> Dimension.width

  static member height (Dimension (_, h)) = h
  member this.Height = this |> Dimension.height

  static member (+) (Dimension (w1, h1), Dimension (w2, h2)) = Dimension (w1 + w2, h1 + h2)
  static member (-) (Dimension (w1, h1), Dimension (w2, h2)) = Dimension (w1 - w2, h1 - h2)
  static member (*) (Dimension (w, h), scale) = Dimension (w * scale, h * scale)

type ImageFilePath =
  | Image of string
  | ImageOfSize of expectedSize:Dimension * string

  static member path path =
    match path with
    | Image path -> path
    | ImageOfSize (_, path) -> path
  member this.Path = this |> ImageFilePath.path

  static member openImage (path:ImageFilePath) =
    let pathString = path |> ImageFilePath.path
    let image =
      (fun () -> new Bitmap(pathString))
      |> Safety.addContext (sprintf "Failed to open image file at '%s'" pathString)

    do
      match path with
      | Image path -> ()
      | ImageOfSize (expectedSize, path) ->
        if image.Width <> expectedSize.Width || image.Height <> expectedSize.Height
        then invalidArg "image" (sprintf "Image '%s' is not of the expected size: %s" path (string expectedSize))

    image

  static member screenshot path = ImageOfSize(Dimension (1920, 1080), path)

/// <summary>
/// A pixel location of a screenshot of Siege.
/// </summary>
type Position = 
  | Position of x:int * y:int

  static member x (Position (x, _)) = x
  member this.X = this |> Position.x

  static member y (Position (_, y)) = y
  member this.Y = this |> Position.y

  static member (+) (Position (x1, y1), Position (x2, y2)) = Position (x1 + x2, y1 + y2)
  static member (-) (Position (x1, y1), Position (x2, y2)) = Position (x1 - x2, y1 - y2)
  static member (*) (Position (x, y), scale) = Position (x * scale, y * scale)

/// <summary>
/// The inclusive bounds of a range of integers.
/// </summary>
type IntRange = 
  | IntRange of min:int * max:int

  static member length (IntRange (min, max)) = max - min + 1
  member this.Length = this |> IntRange.length

  static member (+) (IntRange (min, max), delta) = IntRange (min + delta, max + delta)
  static member (-) (IntRange (min, max), delta) = IntRange (min - delta, max - delta)

  /// <summary>
  /// The increasing, consecutive sequence of integers in the range.
  /// </summary>
  static member seq (IntRange (min, max)) = Seq.init (max - min + 1) (fun i -> i + min)
  /// <summary>
  /// The increasing, consecutive sequence of integers in the range.
  /// </summary>
  member this.Seq = this |> IntRange.seq 

type Row = Row of xs:IntRange * y:int
type Column = Column of x:int * ys:IntRange
type PositionRange = RowCase of Row | ColumnCase of Column

type RGBColor = 
  | RGBColor of r:byte * g:byte * b:byte

  static member ofColor (color:Color) = RGBColor (color.R, color.G, color.B)

  /// <summary>
  /// See ColorDistance.between
  /// </summary>
  static member (|-|) (c1, c2) = ColorDistance.between c1 c2

type ColorDistance =
  | ColorDistance of int

  static member exact = ColorDistance 10
  static member good = ColorDistance 20
  static member passable = ColorDistance 30

  /// <remarks>
  /// Calculates color difference using GIMP's Fuzzy Select (Magic Wand) Tool's "Composite" selection 
  /// criteria: return the absolute value of maximum of the component differences (R, G, or B).
  /// </remarks>
  static member between (RGBColor (r1, g1, b1)) (RGBColor (r2, g2, b2)) =
    ColorDistance <| max3 (abs (int r1 - int r2)) (abs (int g1 - int g2)) (abs (int b1 - int b2))

type DetectableInformation<'T> =
  | Undetected
  | Detected of 'T
type 'T detectable = DetectableInformation<'T>