module Main

open Feliz
open App
open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop

importSideEffects "./styles/main.css"
ReactDOM.render(
    Components.WordleSolver(),
    document.getElementById "feliz-app"
)