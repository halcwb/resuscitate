module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared

type Model2 = Command list * Event list

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { Counter: Counter option }

type Msg2 = Command

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Counter

let initialModel : Model2 =
    let el = [ Observed Unresponsive ]
    let cl = el |> Implementation.getCommands

    cl, el

let initialCounter () = Fetch.fetchAs<Counter> "/api/init"

let init2 () : Model2 * Cmd<Msg2> =
    let initialModel = initialModel
    initialModel, Cmd.none

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None }
    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd

let update2 (msg: Msg2) (currentModel : Model2) : Model2 * Cmd<Msg2> =
    let newModel =
        currentModel
        |> snd
        |> Implementation.processCommand msg

    newModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount->
        let nextModel = { Counter = Some initialCount }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let show = function
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None   } -> "Loading..."

let createButtons dispatch cl =
    cl    
    |> List.map (fun c ->
        button [ OnClick (fun _ -> dispatch c) ] [ str (c |> sprintf "%A") ]
    )

let view2 (model : Model2) (dispatch : Msg2 -> unit) =
    let buttons = model |> fst |> createButtons dispatch
    div []
        ([ h1 [] [ str "Rescitate app" ] ] @ buttons)    


let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ h1 [] [ str "SAFE Template" ]
          p  [] [ str "The initial counter is fetched from server" ]
          p  [] [ str "Press buttons to manipulate counter:" ]
          button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
          div [] [ str (show model) ]
          button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
          safeComponents ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init2 update2 view2
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
