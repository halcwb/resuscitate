module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared

type Model = Command list * Event list

type Msg = Command

let initialModel : Model =
    let el = [ Observed Unresponsive ]
    let cl = el |> Implementation.getCommands

    cl, el

let init () : Model * Cmd<Msg> =
    let initialModel = initialModel
    initialModel, Cmd.none

let update (msg: Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let newModel =
        currentModel
        |> snd
        |> Implementation.processCommand msg

    newModel, Cmd.none


let createButtons dispatch cl =
    cl    
    |> List.map (fun c ->
        button [ OnClick (fun _ -> dispatch c) ] [ str (c |> Protocol.printCommand) ]
    )

let view (model : Model) (dispatch : Msg -> unit) =
    let buttons = 
        model 
        |> fst 
        |> createButtons dispatch
        |> (fun bs ->
            if bs |> List.isEmpty |> not then bs
            else
                model
                |> snd
                |> List.map (Protocol.printEvent)
                |> List.map (fun s -> 
                    p [] [ str s ]
                )
        )

    div []
        ([ h1 [] [ str "Rescitate app" ] ] @ buttons)    


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
