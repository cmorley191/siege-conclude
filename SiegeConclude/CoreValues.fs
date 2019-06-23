namespace SiegeConclude

[<AutoOpen>]
module CoreValues =

  let inline is<'T> x = (box x) :? 'T

  /// <summary>
  /// Maximum of the specified three values based on generic comparison.
  /// </summary>
  let max3 x y z = max (max x y) z

module Union =
  open Microsoft.FSharp.Reflection

  let cases<'T> =
    let cases = FSharpType.GetUnionCases(typeof<'T>, allowAccessToPrivateRepresentation = false)
    [ 
      for c in cases do
        yield FSharpValue.MakeUnion(c, [||]) :?> 'T
    ]
