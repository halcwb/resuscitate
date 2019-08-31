namespace Extensions.Fable.MaterialUI
open Fable.Core
open Fable.Core.JsInterop

module Core =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Themes
    open Fable.MaterialUI.Core

    let inline container (b : IHTMLProp seq) c : ReactElement = ofImport "default" "@material-ui/core/Container" (toObj b) c

    [<AutoOpen>]
    module Props =


        type [<StringEnum; RequireQualifiedAccess>] ContainerMaxWidth =
            | Xs
            | Sm
            | Md
            | Lg
            | Xl
            | [<CompiledName("")>] False

        type ContainerProp =
            | MaxWidth of ContainerMaxWidth
            interface IHTMLProp