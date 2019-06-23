module SiegeConclude.SiegeModel

open System
open SiegeConclude.SiegeTypes
open SiegeConclude.SiegeInterfaceElements

type ContradictionException(message:string) =
  inherit Exception(message)

/// Raise a ContradictionException.
let contradiction message = raise (ContradictionException(message))

module OperatorSelectionPhase =
  open SiegeConclude.SiegeInterfaceElements.OperatorSelectPhase

  module StatusBar =
    open SiegeConclude.SiegeInterfaceElements.OperatorSelectPhase.StatusBar

    module PlayerStatusIndicator =
      open SiegeConclude.SiegeInterfaceElements.OperatorSelectPhase.StatusBar.PlayerStatusIndicator

      type AllyInformation = AllyInformation of Operator
      type EnemyInformation = NoEnemyInformationAvailable

      type Indicator<'info> =
        | Empty
        | Unknown
        | Ready of 'info
        | Loading of 'info

      type PlayerIndicators =
        {
          AllyPlayerIndicators: Indicator<AllyInformation> detectable array
          EnemyPlayerIndicators: Indicator<EnemyInformation> detectable array
        }

      let Default =
        {
          AllyPlayerIndicators = Array.replicate 5 Undetected
          EnemyPlayerIndicators = Array.replicate 5 Undetected
        }

      let private blueEmptyElement = IESpec.load (PlayerStatusIndicator.empty Blue)
      let private orangeEmptyElement = IESpec.load (PlayerStatusIndicator.empty Orange)
      let private blueUnknownElement = IESpec.load (PlayerStatusIndicator.unknown Blue)
      let private orangeUnknownElement = IESpec.load (PlayerStatusIndicator.unknown Orange)
      let private blueEnemyReadyElement = IESpec.load (PlayerStatusIndicator.enemyReady Blue)
      let private orangeEnemyReadyElement = IESpec.load (PlayerStatusIndicator.enemyReady Orange)
      let private operatorElements =
        PlayerStatusIndicator.OperatorIcon.operators
        |> List.map (fun (op, template) -> (op, IESpec.load template))
      let detect =
        let emptyElement = function | Blue -> blueEmptyElement | Orange -> orangeEmptyElement
        let unknownElement = function | Blue -> blueUnknownElement | Orange -> orangeUnknownElement
        let enemyReadyElement = function | Blue -> blueEnemyReadyElement | Orange -> orangeEnemyReadyElement

        fun allyTeamColor screenshot -> 
          let enemyTeamColor = allyTeamColor |> TeamColor.other
          {
            AllyPlayerIndicators =
              [| 0 .. 4 |]
              |> Array.map (fun iPlayer -> 
                let offset = offset Ally iPlayer
                if InterfaceAnalyzer.identifyOffsetElement offset (emptyElement allyTeamColor) screenshot
                then Detected Empty
                elif InterfaceAnalyzer.identifyOffsetElement offset (unknownElement allyTeamColor) screenshot
                then Detected Unknown
                else
                  match
                    operatorElements
                    |> Seq.where (fun (op, element) -> InterfaceAnalyzer.identifyOffsetElement offset element screenshot)
                    |> Seq.tryHead with
                  | Some (op, _) -> Detected (Ready (AllyInformation op))
                  | None ->
                    Undetected
              )

            EnemyPlayerIndicators =
              [| 0 .. 4|]
              |> Array.map (fun iPlayer ->
                let offset = offset Enemy iPlayer
                if InterfaceAnalyzer.identifyOffsetElement offset (emptyElement enemyTeamColor) screenshot
                then Detected Empty
                elif InterfaceAnalyzer.identifyOffsetElement offset (unknownElement enemyTeamColor) screenshot
                then Detected Unknown
                elif InterfaceAnalyzer.identifyOffsetElement offset (enemyReadyElement enemyTeamColor) screenshot
                then Detected (Ready NoEnemyInformationAvailable)
                else Undetected
              )
          }

      let loadModule() = 
        [
          blueEmptyElement
          orangeEmptyElement
          blueUnknownElement
          orangeUnknownElement
          blueEnemyReadyElement
          orangeEnemyReadyElement 
        ] |> ignore
        operatorElements |> ignore

    type StatusBar =
      {
        PlayerIndicators: PlayerStatusIndicator.PlayerIndicators
      }

    let Default = 
      { 
        PlayerIndicators = PlayerStatusIndicator.Default 
      }

    let detect =
      fun allyTeamColor screenshot ->
        {
          PlayerIndicators = PlayerStatusIndicator.detect allyTeamColor screenshot
        }

    let private allyBlueElement = IESpec.load (StatusBar.bar Ally Blue)
    let private allyOrangeElement = IESpec.load (StatusBar.bar Ally Orange)
    let private enemyBlueElement = IESpec.load (StatusBar.bar Enemy Blue)
    let private enemyOrangeElement = IESpec.load (StatusBar.bar Enemy Orange)
    let detectAllyTeamColor =
      let barElement =
        function
        | Ally -> function | Blue -> allyBlueElement | Orange -> allyOrangeElement
        | Enemy -> function | Blue -> enemyBlueElement | Orange -> enemyOrangeElement

      fun screenshot ->
        let rec checkGuess firstGuess (guessColor:TeamColor) = 
          if InterfaceAnalyzer.identifyElement (barElement Enemy guessColor.Other) screenshot then
            if InterfaceAnalyzer.identifyElement (barElement Ally guessColor) screenshot 
            then Some <| Detected guessColor
            elif InterfaceAnalyzer.identifyElement (barElement Ally guessColor.Other) screenshot
            then contradiction (sprintf "Both sides of the operator selection status bar are %s." (string guessColor))
            else Some <| Undetected
          elif firstGuess then checkGuess false guessColor.Other 
          else None
        
        match Blue |> checkGuess true with
        | Some result -> result
        | None -> Undetected

    let loadModule() =
      [
        allyBlueElement
        allyOrangeElement
        enemyBlueElement
        enemyOrangeElement
      ] |> ignore

      PlayerStatusIndicator.loadModule()

  type OperatorSelectionScreenshot = 
    {
      AllyTeamColor: TeamColor detectable
      StatusBar: StatusBar.StatusBar
    }

  let Default =
    {
      AllyTeamColor = Undetected
      StatusBar = StatusBar.Default
    }

  let detect =
    fun screenshot ->      
      let allyTeamColor = StatusBar.detectAllyTeamColor screenshot
      
      match allyTeamColor with
      | Undetected -> { Default with AllyTeamColor = Undetected }
      | Detected allyTeamColor as AllyTeamColor ->
        let StatusBar = StatusBar.detect allyTeamColor screenshot
        {
          AllyTeamColor = AllyTeamColor
          StatusBar = StatusBar
        }
    
  let loadModule() =
    StatusBar.loadModule()

type ScreenshotModel =
  | OperatorSelect of OperatorSelectionPhase.OperatorSelectionScreenshot

let detect =
  fun screenshot ->
    OperatorSelect (OperatorSelectionPhase.detect screenshot)

let loadModule() =
  OperatorSelectionPhase.loadModule()