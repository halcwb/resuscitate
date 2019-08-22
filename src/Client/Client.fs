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

// == HELPER FUNCTIONS ==


// === MODEL ===

type Model = 
    { 
        Description : Description list
        Commands : Command list
        Events: Event list
    }

type Msg =
    | NavBarMsg
    | CommandMsg of Command

let initialModel : Model =
    let es = [ Observed Unresponsive ]
    let ds, cs =
        es
        |> Implementation.getCommands
    {
        Description = ds
        Commands = cs
        Events = es
    }

let init () : Model * Cmd<Msg> =
    let initialModel = initialModel
    initialModel, Cmd.none

let update (msg: Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | NavBarMsg -> init ()
    | CommandMsg cmd ->
        let ds, cs, es =
            currentModel.Events
            |> Implementation.processCommand cmd

        {
            Description = ds
            Commands = cs
            Events = es
        } , Cmd.none


// === STYLES ===

let mainDivStyle = Style [ CSSProp.Padding "20px" ]

let flexColumnStyle =
    Style [ CSSProp.Display DisplayOptions.Flex
            CSSProp.FlexDirection "column" ] 
    

let bodyContainerStyle = 
    Style [ CSSProp.Top "0"
            CSSProp.MarginTop "60px"
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
        model.Commands 
        |> createButtons (CommandMsg >> dispatch)
        |> (fun bs ->
            if bs |> List.isEmpty |> not then 
                bs
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
        )

    [ header; cmds ] |> bodyContainer bodyContainerStyle

let view (model : Model) (dispatch : Msg -> unit) =

    div [ mainDivStyle ]
        [ 
            yield dispatch |> navBar 
            yield model |> createBody dispatch 
        ]  


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
