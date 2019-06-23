module SiegeConclude.SiegeTypes

type Position = 
  | Attack | Defend

  static member other = function | Attack -> Defend | Defend -> Attack
  member this.Other = this |> Position.other

type Playlist = Newcomer | Casual | Ranked | Custom
type Gamemode = Bomb | Secure | Hostage

type TeamColor = 
  | Blue | Orange

  static member other = function | Blue -> Orange | Orange -> Blue
  member this.Other = this |> TeamColor.other

type Alignment = 
  | Ally | Enemy

  static member other = function | Ally -> Enemy | Enemy -> Ally
  member this.Other = this |> Alignment.other

type Year = Release | Year1 | Year2 | Year3 | Year4

type Map = 
  | House | Oregon | HerefordBase | ClubHouse | PresidentialPlane
  | Consulate | Bank | Kanal | Chalet | KafeDostoyevsky
  | Yacht | Border | Favela | Skyscraper
  | Coastline | ThemePark | Tower
  | Villa | Fortress
  | Outback

type Organization = 
  | SAS | FBISWAT | GIGN | Spetsnaz | GSG9 
  | JTF2 | SEALs | BOPE | SAT
  | GEO | SDU | GROM | SMB707 
  | CBRN | GIS | GSUTR | GIGR
  | SASR | JaegerCorps | SecretService

  static member year org =
    match org with
    | SAS | FBISWAT | GIGN | Spetsnaz | GSG9 -> Release
    | JTF2 | SEALs | BOPE | SAT -> Year1
    | GEO | SDU | GROM | SMB707 -> Year2
    | CBRN | GIS | GSUTR | GIGR -> Year3
    | SASR | JaegerCorps | SecretService -> Year4
  member this.Year = this |> Organization.year

  static member color org : RGBColor =
    failwith "not implemented yet"
  member this.Color = this |> Organization.color

type Operator = 
  | Sledge | Thatcher | Smoke | Mute
  | Ash | Thermite | Castle | Pulse
  | Twitch | Montagne | Doc | Rook
  | Glaz | Fuze | Kapkan | Tachanka
  | Blitz | IQ | Jager | Bandit
  | Buck | Frost
  | Blackbeard | Valkyrie
  | Capitao | Caveira
  | Hibana | Echo
  | Jackal | Mira
  | Ying | Lesion
  | Zofia | Ela
  | Dokkaebi | Vigil
  | Lion | Finka
  | Maestro | Alibi
  | Maverick | Clash
  | Nomad | Kaid
  | Gridlock | Mozzie
  | Nokk | Warden

  static member position op =
    match op with
    | Sledge | Thatcher | Ash | Thermite | Twitch | Montagne  | Glaz | Fuze  | Blitz | IQ 
    | Buck  | Blackbeard | Capitao | Hibana
    | Jackal | Ying | Zofia | Dokkaebi 
    | Lion | Maestro | Maverick | Nomad 
    | Gridlock | Nokk
      -> Attack
    | Smoke | Mute | Castle | Pulse | Doc | Rook | Kapkan | Tachanka | Jager | Bandit
    | Frost | Valkyrie | Caveira | Echo
    | Mira | Lesion | Ela | Vigil
    | Finka | Alibi | Clash | Kaid
    | Mozzie | Warden
      -> Defend
  member this.Position = this |> Operator.position

  static member organization op =
    match op with
    | Sledge | Thatcher | Smoke | Mute -> SAS
    | Ash | Thermite | Castle | Pulse -> FBISWAT
    | Twitch | Montagne | Doc | Rook -> GIGN
    | Glaz | Fuze | Kapkan | Tachanka -> Spetsnaz
    | Blitz | IQ | Jager | Bandit -> GSG9
    | Buck | Frost -> JTF2
    | Blackbeard | Valkyrie -> SEALs
    | Capitao | Caveira -> BOPE
    | Hibana | Echo -> SAT
    | Jackal | Mira -> GEO
    | Ying | Lesion -> SDU
    | Zofia | Ela -> GROM
    | Dokkaebi | Vigil -> SMB707
    | Lion | Finka -> CBRN
    | Maestro | Alibi -> GIS
    | Maverick | Clash -> GSUTR
    | Nomad | Kaid -> GIGR
    | Gridlock | Mozzie -> SASR
    | Nokk -> JaegerCorps
    | Warden -> SecretService
  member this.Organization = this |> Operator.organization

type PlayableCharacter = Recruit | NotRecruit of Operator