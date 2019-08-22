module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Fable.MaterialUI.Core
open Fable.MaterialUI.Props
open Fable.MaterialUI.Themes

open Shared

module Timer =

    open System
    open Browser
    
    type Model = 
        {
            Start : DateTime
            Current: DateTime
        }

    type Msg = Tick of DateTime

    let tick dispatch =
        window.setInterval(fun _ ->
            dispatch (Tick DateTime.Now)
        , 1000) |> ignore

    let init () =
        let now = DateTime.Now
        {
            Start = now
            Current = now
        }

    let secondsPast model =
        (model.Current - model.Start).Duration()

    let update (Tick next) model =
        { model with Current = next }
        

// == HELPER FUNCTIONS ==


// === MODEL ===

type Model = 
    { 
        Description : Description list
        Commands : Command list
        Events: Event list
        Duration : Timer.Model Option
    }

type Msg =
    | NavBarMsg
    | CommandMsg of Command
    | TimerMsg of Timer.Msg


let init () : Model * Cmd<Msg> =
    let initialModel = 
        let es = [ Observed Unresponsive ]
        let ds, cs =
            es
            |> Implementation.getCommands
        {
            Description = ds
            Commands = cs
            Events = es
            Duration = None
        }

    initialModel, Cmd.none

let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | NavBarMsg -> init ()
    | CommandMsg cmd ->
        let model =
            match cmd with
            | Intervene CPR2Min -> 
                {
                    model with
                        Duration = Timer.init () |> Some
                }
            | Intervene CPRStop ->
                {
                    model with
                        Duration = None
                }
            | _ -> model

        let ds, cs, es =
            model.Events
            |> Implementation.processCommand cmd

        {   
            model with
                Description = ds
                Commands = cs
                Events = es
        } , Cmd.none
    | TimerMsg msg ->
        { 
            model with
                Duration = 
                    model.Duration 
                    |> Option.map (Timer.update msg)
        }, Cmd.none



// === STYLES ===

let mainDivStyle = Style [ CSSProp.Padding "20px" ]

let flexColumnStyle =
    Style [ CSSProp.Display DisplayOptions.Flex
            CSSProp.FlexDirection "column" ] 
    

let bodyContainerStyle = 
    Style [ CSSProp.Top "0"
            CSSProp.MarginTop "60px"
            CSSProp.Padding "20px"
            CSSProp.Display DisplayOptions.Flex
            CSSProp.FlexDirection "column" ]

let commandButtonsStyle c =
    let color =
        match c with
        | Observe _ -> Fable.MaterialUI.Colors.green.``100``
        | Intervene _ -> Fable.MaterialUI.Colors.blue.``100``

    Style [ CSSProp.Padding "20px"
            CSSProp.Margin "10px"
            CSSProp.BackgroundColor color ] 


// === VIEW FUNCIONS ===

let createButtons dispatch cl =
    cl    
    |> List.map (fun c ->
        let style = c |> commandButtonsStyle
        button [ OnClick (fun _ -> dispatch c)
                 ButtonProp.Variant ButtonVariant.Contained
                 style ] [ str (c |> Protocol.printCommand) ]
    )


let navBar dispatch = Views.NavBar.view "GenAPLS" NavBarMsg dispatch


let bodyContainer style body =
    div [ style ]  body 


let createHeader model =
    if model.Description |> List.isEmpty then
        [ "This is what happened:" ]
    else
        model.Description
    |> List.map (fun s ->
        s
        |> str
        |> List.singleton
        |> typography [ TypographyProp.Variant TypographyVariant.H5 ]
        |> List.singleton
        |> listItem []
    )
    |> list []


let createBody dispatch model = 
    let header =
        model |> createHeader

    let cmds =
        if model.Commands |> List.isEmpty |> not then 
            model.Commands
            |> createButtons (CommandMsg >> dispatch)
            |> div [ flexColumnStyle ]
        else
            model.Events
            |> List.map (Protocol.printEvent)
            |> List.mapi (fun i s -> 
                let s = sprintf "%i. %s" (i + 1) s
                listItem [] 
                            [ listItemText [] [ str s ] ]
            )
            |> list []

    let duration =
        model.Duration
        |> Option.map (fun d -> 
            d
            |> Timer.secondsPast
            |> fun ts -> 
                sprintf "Duration: %i minutes %i seconds" ts.Minutes ts.Seconds
                |> str
                |> List.singleton
            |> typography [ TypographyProp.Variant TypographyVariant.H6
                            TypographyProp.Color TypographyColor.TextSecondary
                            Style [ CSSProp.MarginLeft "10px" 
                                    CSSProp.MarginBottom "20px"
                                    CSSProp.Position PositionOptions.Fixed
                                    CSSProp.Bottom "0" ] ]
        )

    [ 
        yield header
        yield cmds
        if duration |> Option.isSome then yield duration |> Option.get 
    ] |> bodyContainer bodyContainerStyle

let view (model : Model) (dispatch : Msg -> unit) =

    div [ mainDivStyle ]
        [ 
            yield dispatch |> navBar 
            yield model |> createBody dispatch 
        ]  

let subscription _ =
    Cmd.batch [
        Cmd.map TimerMsg (Cmd.ofSub Timer.tick)
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription subscription
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
