module SiegeConclude.SiegeInterfaceElements

open SiegeConclude.SiegeTypes

open System.IO

let private templateDirectory = "IESpecTemplates"
let private templateFolder = templateDirectory
let private templateFileName = ""
let private (+++) fname moduleAbbreviation = 
  if fname = "" 
  then moduleAbbreviation
  else fname + "." + moduleAbbreviation
let private (+/) path1 path2 = Path.Combine(path1, path2)

module OperatorSelectPhase =
  let private templateFileName = templateFileName +++ "opSelect"

  module StatusBar =
    let private templateFileName = templateFileName +++ "statusBar"

    let private barPosition = 
      function
      | Ally -> Position (548, 65)
      | Enemy -> Position (1130, 65)

    let private primaryColor = 
      function
      | Blue -> RGBColor (0x06uy, 0x73uy, 0xc9uy)
      | Orange -> RGBColor (0xd6uy, 0x63uy, 0x0auy)
    let emptyColor =
      function
      | Blue -> RGBColor (0x0duy, 0x36uy, 0x56uy)
      | Orange -> RGBColor (0x52uy, 0x30uy, 0x14uy)
    let private unknownColor =
      function
      | Blue -> RGBColor (0x03uy, 0x4auy, 0x82uy)
      | Orange -> RGBColor (0x8buy, 0x41uy, 0x06uy)

    let bar alignment tColor =
      {
        TemplatePath = Image <| templateFolder +/ templateFileName +++ (match alignment with | Ally -> "ally" | Enemy -> "enemy") + ".bmp"
        Offset = 
          match alignment with
          | Ally -> barPosition Ally + (Position (190, -5))
          | Enemy -> barPosition Enemy + (Position (-13, -3))
        PaintChecks = 
          let basicPaints =
            [
              (Paints.red, { Logic = Expect (primaryColor tColor); Threshold = ColorDistance.good })
              (Paints.green, { 
                Logic =
                  match alignment with
                  | Ally -> ExpectAny [primaryColor tColor; unknownColor tColor]
                  | Enemy -> Expect (unknownColor tColor)
                Threshold = ColorDistance.exact
              })
              (Paints.blue, { Logic = ExpectNotAny [primaryColor tColor; unknownColor tColor]; Threshold = ColorDistance.passable} )
            ]
          match alignment with
          | Ally -> 
            (Paints.yellow, { Logic = Expect (RGBColor (0xfcuy, 0xffuy, 0xffuy)); Threshold = ColorDistance.exact})
              :: basicPaints
          | Enemy -> basicPaints
      }

    module PlayerStatusIndicator =
      let private templateFileName = templateFileName +++ "playerStatus"

      let private size = Dimension (45, 52)
      let private indicatorRegionOffset = Position (2, 0)
      let private spacing = Position (48, 0)

      let offset alignment iPlayer = (barPosition alignment) + indicatorRegionOffset + spacing * iPlayer

      let empty tColor =
        {
          TemplatePath = ImageOfSize (size, templateDirectory +/ templateFileName +++ "empty.bmp")
          Offset = Position (0, 0)
          PaintChecks = [
            (Paints.red, { Logic = Expect (emptyColor tColor); Threshold = ColorDistance.exact })
          ]
        }

      let unknown tColor = 
        {
          TemplatePath = ImageOfSize (size, templateDirectory +/ templateFileName +++ "unknown.bmp")
          Offset = Position (0, 0)
          PaintChecks = [
            (Paints.green, { Logic = Expect (RGBColor (0xffuy, 0xfduy, 0xf4uy)); Threshold = ColorDistance.good } )
            (Paints.red, { Logic = Expect (unknownColor tColor); Threshold = ColorDistance.exact })
          ]
        }

      let enemyReady tColor =
        {
          TemplatePath = ImageOfSize (size, templateDirectory +/ templateFileName +++ "enemyReady.bmp")
          Offset = Position (0, 0)
          PaintChecks = [
            (Paints.green, { Logic = Expect (RGBColor (0xffuy, 0xffuy, 0xf9uy)); Threshold = ColorDistance.good } )
            (Paints.red, { Logic = Expect (unknownColor tColor); Threshold = ColorDistance.exact })
          ]
        }

      module OperatorIcon =
        let private templateFileName = templateFileName +++ "opIcon"

        let operator =
          function
          | Frost ->
            Some <|
              { 
                TemplatePath = ImageOfSize (size, templateDirectory +/ (templateFileName +++ "frost.bmp"))
                Offset = Position (0, 0)
                PaintChecks = [
                  (Paints.red, { Logic = Expect (RGBColor (5uy, 119uy, 151uy)); Threshold = ColorDistance.good })
                  (Paints.green, { Logic = Expect (RGBColor (249uy, 251uy, 248uy)); Threshold = ColorDistance.exact })
                  (Paints.blue, { Logic = Expect (RGBColor (0uy, 1uy, 0uy)); Threshold = ColorDistance.exact})
                ]
              }
          | _ -> None

        let operators =
          Union.cases<Operator>
          |> List.map (fun op -> (op, operator op))
          |> List.choose (function | (op, Some template) -> Some (op, template) | (_, None) -> None)

    module TeamPositionIcon =
      begin end

    module TeamScoreIndicator =
      begin end

    module PhaseTimer =
      begin end

    module RoundCounter = 
      type Counter =
        | Round of int
        | MatchPoint
        | Overtime
        | OvertimeMatchPoint
      
    
