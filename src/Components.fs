namespace App

open Feliz
open Feliz.Router
open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
module Data =
    [<ImportDefault("./words.json")>]
    let words: string [] = jsNative

type FixedLetter = {
    Letter: string
    Position: int
}

type IncludeLetter = {
    Letter: string
    InvalidPositions: int list
}
type WordleState = 
    {
        Words: string array
        FixedLetters: Map<int, string>
        ExcludeLetters: string
        IncludeLetters: IncludeLetter list
        Filter: string
    }
    with
        static member filterCandidates (state: WordleState) =
            let excludedCharactersArr = state.ExcludeLetters.ToCharArray() |> Array.map (fun x -> x.ToString())
            let wordList = state.Words |> List.ofArray
            let fixedLetters = state.FixedLetters
            let includeLetters = state.IncludeLetters
            let result = 
                wordList
                |> List.filter (fun w -> 
                    let chars = w.ToCharArray() |> Array.map (fun x -> x.ToString())
                    let letterComparison = 
                        [ for i in 0 .. 4 do
                            let currentChar = chars[i]
                            if excludedCharactersArr |> Array.contains currentChar then yield false
                            else if fixedLetters |> Map.containsKey i && fixedLetters[i] <> currentChar then yield false
                            else yield true
                        ] |> List.contains false |> not
                    let wordIndicesLookup = chars |> Array.mapi (fun x c -> c,x) |> Array.groupBy fst |> Map.ofArray |> Map.map (fun _ grp -> grp |> Array.map snd |> Set.ofArray)
                    let includeComparison =
                        includeLetters
                        |> List.map (fun includeLetter ->
                            let letter = includeLetter.Letter
                            let indices = includeLetter.InvalidPositions |> Set.ofList
                            let letterIndices = wordIndicesLookup |> Map.tryFind letter |> Option.defaultValue Set.empty
                            let diff = Set.difference letterIndices indices
                            diff |> Set.isEmpty |> not
                        ) |> List.contains false |> not
                    letterComparison && includeComparison
                )
            result

type Components =
    [<ReactComponent>]
    static member WordleSolver() = 
        let (state, setState) =
            React.useState(
                {
                    Words = Data.words
                    FixedLetters = Map.empty
                    ExcludeLetters = ""
                    IncludeLetters = []
                    Filter = ""
                })
        let candidates = WordleState.filterCandidates state
        let filteredCandidates = candidates |> List.filter (fun w -> w.Contains(state.Filter))
        console.log("filtered candidates", Data.words)
        Html.div [
            Html.h1 "Filtered candidates"
            Html.div [
                Html.label [
                    Html.text "Solved"
                ]
                Html.div [ 
                    for i in 0 .. 4 do
                        yield Html.input [
                            prop.value (state.FixedLetters |> Map.tryFind i |> Option.defaultValue "")
                            prop.maxLength 1
                            prop.onChange (fun (v: string) ->
                                let fixedLetters = state.FixedLetters |> Map.add i (v.ToUpper())
                                setState { state with FixedLetters = fixedLetters })
                        ]
                ]
                Html.label [
                    Html.text "Include"
                ]
                Html.button [
                    prop.text "Add include letter"
                    prop.onClick (fun _ ->
                        let includeLetters = state.IncludeLetters @ [{ Letter = ""; InvalidPositions = [] }]
                        setState { state with IncludeLetters = includeLetters })
                ]
                Html.div 
                    (state.IncludeLetters
                    |> List.mapi (fun index includeLetter ->
                        Html.div [
                            Html.input [
                                prop.value includeLetter.Letter
                                prop.maxLength 1
                                prop.onChange (fun (v: string) -> 
                                    let includeLetters =
                                        state.IncludeLetters
                                        |> List.mapi (fun i il' ->
                                            if i = index then { Letter = v.ToUpper(); InvalidPositions = il'.InvalidPositions }
                                            else il'
                                        )
                                    setState { state with IncludeLetters = includeLetters })
                            ]
                            Html.div
                                [ for i in 0 .. 4 do
                                    yield Html.input [
                                        prop.type' "checkbox"
                                        prop.isChecked (includeLetter.InvalidPositions |> List.contains i)
                                        prop.onCheckedChange (fun v ->
                                            let invalidPositions' = 
                                                if v then i::includeLetter.InvalidPositions
                                                else includeLetter.InvalidPositions |> List.filter (fun x -> x <> i)
                                            let includeLetter' = { Letter = includeLetter.Letter; InvalidPositions = invalidPositions' }
                                            let includeLetters' = 
                                                state.IncludeLetters
                                                |> List.mapi (fun i il' ->
                                                    if i = index then includeLetter'
                                                    else il'
                                                )
                                            console.log("IL: ", includeLetters')
                                            setState({state with IncludeLetters = includeLetters'})
                                        )
                                    ]
                                ]
                        ]
                    ))
                Html.label [
                    Html.text "Filter: "
                ]
                Html.input [
                    prop.onChange (fun (v: string) -> 
                        let upper = v.ToUpper()
                        setState({ state with Filter = upper}))
                ]
                Html.label [
                    Html.text "Exclude letters: "
                ]
                Html.input [
                    prop.onChange (fun (v: string) -> 
                        let upper = v.ToUpper()
                        setState({ state with ExcludeLetters = upper}))
                ]
            ]
            Html.ul [
                for w in filteredCandidates do yield Html.li w
            ]
        ]

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
            ]
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Html.h1 "Index"
                | [ "counter" ] -> Components.Counter()
                | otherwise -> Html.h1 "Not found"
            ]
        ]