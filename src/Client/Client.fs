module Client

open System
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

open Extensions.Fable.MaterialUI.Core

open Shared
open Views

module Speech =

    open Fable.Core

    [<Emit("window.speechSynthesis.speak(new SpeechSynthesisUtterance($0));")>]
    let speak s = ()


module Timer =

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
        WaitFor : int Option
        Volume : bool
    }

type Msg =
    | NavBarMsg of NavBar.Msg
    | CommandMsg of Command
    | TimerMsg of Timer.Msg


let init () : Model * Cmd<Msg> =
    let initialModel = 
        {
            Description = [ "Start the protocol when the patient is Non Responsive", "" ]
            Commands = [ Observe NonResponsive ]
            Events = []
            Duration = None
            WaitFor = None
            Volume = true
        }

    initialModel, Cmd.none

let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | NavBarMsg msg -> 
        match msg with
        | NavBar.Reset -> 
            let newModel, cmd = init()
            { newModel with Volume = model.Volume }, cmd

        | NavBar.ToggleVolume ->
            if model.Volume |> not then
                Speech.speak "Volume is now on"
            { model with Volume = model.Volume |> not }, []

    | CommandMsg cmd ->
        let model =
            match cmd with
            | Intervene CPR2MinStart -> 
                {
                    model with
                        Duration = Timer.init () |> Some
                }

            | Intervene (CPR2MinStop _)  ->
                {
                    model with
                        Duration = None
                }

            | _ -> model

        let { Descriptions = ds; Commands = cs; Events = es}  =
            model.Events
            |> Implementation.processCommand cmd

        if model.Volume then
            if cs |> List.isEmpty then 
                "The protocol has finished, results are shown"
                |> Speech.speak
            else
                ds 
                |> List.map snd
                |> List.filter (fun s -> s |> String.IsNullOrWhiteSpace |> not)
                |> List.iter Speech.speak 

        {   
            model with
                Description = ds
                Commands = cs
                Events = es
                WaitFor = 
                    match cs with
                    | [ Intervene(ChargeDefib x) ] -> Some x
                    | [ Intervene(CPR2MinStop x) ] -> Some x
                    | _ -> None
        } , Cmd.none

    | TimerMsg msg ->
        { 
            model with
                Duration = 
                    model.Duration 
                    |> Option.map (Timer.update msg)
                WaitFor = 
                    model.WaitFor
                    |> function 
                    | Some x -> 
                        match model.Duration with
                        | None   -> None
                        | Some d -> 
                            let n = (d.Current - d.Start).TotalSeconds |> int
                            if x - n > 0 then x |> Some
                            else x - n |> Some

                    | None -> None
        }, Cmd.none



// === STYLES ===

let mainDivStyle = Style [ CSSProp.Padding "20px" ]

let flexColumnStyle =
    Style [ CSSProp.Display DisplayOptions.Flex
            CSSProp.FlexDirection "column" ] 
     

let bodyContainerStyle = 
    Style [ CSSProp.Top "0"
            CSSProp.MarginTop "40px"
            CSSProp.Padding "20px"
            CSSProp.Display DisplayOptions.Flex
            CSSProp.FlexDirection "column" ]

let commandButtonsStyle w c =
    let color =
        match c with
        | Observe _ -> Fable.MaterialUI.Colors.green.``100``
        | Intervene _ ->
            match w with
            | Some x when x < 0 -> 
                if x % 2 = 0 then Fable.MaterialUI.Colors.blue.``100``
                else Fable.MaterialUI.Colors.red.``300``
            | _ -> Fable.MaterialUI.Colors.blue.``100``      

    Style [ CSSProp.Padding "20px"
            CSSProp.Margin "10px"
            CSSProp.BackgroundColor color ] 


// === VIEW FUNCIONS ===

let createButtons waitFor dispatch cl =             
    cl    
    |> List.map (fun c ->
        let style = c |> commandButtonsStyle waitFor
        button [ OnClick (fun _ -> dispatch c)                 
                 ButtonProp.Variant ButtonVariant.Contained
                 style ] [ str (c |> Protocol.printCommand) ]
    )


let navBar model dispatch = 
    Views.NavBar.view 
        "GenAPLS" 
        model.Volume 
        (NavBarMsg >> dispatch)


let bodyContainer style body =
    container [ style; MaxWidth ContainerMaxWidth.Sm ]  body 


let createDescription  model =
    if model.Description |> List.isEmpty then []
    else
        model.Description
        |> List.map fst
        |> List.skip 1
        |> List.map (fun s ->
            s
            |> str
            |> List.singleton
            |> typography [ TypographyProp.Variant TypographyVariant.Body1 ]
            |> List.singleton
            |> listItem []
        )
    |> list []


let createHeader model =
    if model.Description |> List.isEmpty then
        let start = 
            model.Events
            |> List.map (fun e -> 
                match e with
                | Observed x   -> x.DateTime
                | Intervened x -> x.DateTime
            )
            |> List.rev
            |> List.tryHead
            |> Option.defaultValue DateTime.Now
        [ (sprintf "On %s. This is what happened:" (start.ToString("dd-MM-yy"))) ]
    else
        model.Description
        |> List.map fst
        |> List.take 1
    |> List.map (fun s ->
        s
        |> str
        |> List.singleton
        |> typography [ TypographyProp.Variant TypographyVariant.H6 ]
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
            |> createButtons model.WaitFor (CommandMsg >> dispatch)
            |> div [ flexColumnStyle ]
        else
            model.Events
            |> List.map (fun e -> 
                listItem [] 
                         [ listItemText [] [ str (e |> Protocol.printEvent) ] ]
            )             |> list []

    let description = model |> createDescription

    let duration =
        model.Duration
        |> Option.map (fun d -> 
            d
            |> Timer.secondsPast
            |> fun ts -> 
                sprintf "CPR Duration: %i minutes %i seconds" ts.Minutes ts.Seconds
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
        yield description
        if duration |> Option.isSome then 
            yield duration 
                  |> Option.get 
    ] |> bodyContainer bodyContainerStyle

let view (model : Model) (dispatch : Msg -> unit) =

    div [ mainDivStyle ]
        [ 
            yield dispatch |> navBar model
            yield model    |> createBody dispatch 
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
