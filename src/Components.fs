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
        Words: string list
        FixedLetters: Map<int, string>
        ExcludeLetters: string
        IncludeLetters: IncludeLetter list
        Filter: string
    }
    with
        static member filterCandidates (state: WordleState) =
            let excludedCharactersArr = state.ExcludeLetters.ToCharArray() |> Array.map (fun x -> x.ToString())
            let wordList = state.Words
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
                    Words = Data.words |> List.ofArray
                    FixedLetters = Map.empty
                    ExcludeLetters = ""
                    IncludeLetters = []
                    Filter = ""
                })
        let candidates = WordleState.filterCandidates state
        let filteredCandidates = candidates |> List.filter (fun w -> w.Contains(state.Filter))
        Html.div [
            prop.className "flex flex-col gap-4"
            prop.children [
                Html.h1 [
                    prop.text "Wordle Solver"
                    prop.className "w-full text-center font-bold text-4xl"
                ]
                Html.div [
                    Html.label [
                        Html.h2 "Solved"
                    ]
                    Html.div [
                        prop.className "flex gap-2 justify-around"
                        prop.children
                            [ for i in 0 .. 4 do
                                yield 
                                    Html.div [
                                        prop.className "flex-0 w-12"
                                        prop.children [
                                            Html.input [
                                                prop.className "rounded w-full text-gray-800 text-center"
                                                prop.value (state.FixedLetters |> Map.tryFind i |> Option.defaultValue "")
                                                prop.maxLength 1
                                                prop.onChange (fun (v: string) ->
                                                    let fixedLetters = state.FixedLetters |> Map.add i (v.ToUpper())
                                                    setState { state with FixedLetters = fixedLetters })
                                            ]
                                        ]
                                    ]
                            ]
                    ]
                ]
                Html.div [
                    Html.label [
                        Html.h2 "Exclude letters: "
                    ]
                    Html.input [
                        prop.className "w-full rounded text-gray-800"
                        prop.onChange (fun (v: string) -> 
                            let upper = v.ToUpper()
                            setState({ state with ExcludeLetters = upper}))
                    ]

                ]
                Html.div [
                    prop.className "flex gap-2 flex-col"
                    prop.children [
                        Html.label [
                            Html.text "Include letters"
                        ]
                        Html.div [
                            prop.className "flex flex-col gap-2"
                            prop.children
                                (state.IncludeLetters
                                |> List.mapi (fun index includeLetter ->
                                    Html.div [
                                        prop.className "flex gap-2"
                                        prop.children [
                                            Html.div [
                                                prop.className "flex gap-1 flex-col"
                                                prop.children [
                                                    Html.label [ prop.text "Letter:"]
                                                    Html.input [
                                                        prop.className "rounded w-12 text-gray-800 text-center"
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
                                                ]
                                            ]
                                            Html.div [
                                                prop.className "flex-1 flex flex-col gap-1"
                                                prop.children [
                                                    Html.label [ prop.text "Invalid positions:"]
                                                    Html.div [
                                                        prop.className "flex gap-1 justify-between"
                                                        prop.children
                                                            [ for i in 0 .. 4 do
                                                                yield 
                                                                    Html.label [
                                                                        prop.htmlFor (sprintf "invalid-position-%i-%i" index i)
                                                                        prop.children [
                                                                            Html.div [
                                                                                prop.className (sprintf "rounded w-12 h-12 p-3 leading-12 %s text-center" (if includeLetter.InvalidPositions |> List.contains i then "bg-red-700" else "bg-green-700"))
                                                                                prop.text (sprintf "%i" (i+1))
                                                                            ]
                                                                            Html.input [
                                                                                prop.className "invisible"
                                                                                prop.name (sprintf "invalid-position-%i-%i" index i)
                                                                                prop.id (sprintf "invalid-position-%i-%i" index i)
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
                                                                                    setState({state with IncludeLetters = includeLetters'})
                                                                                )
                                                                            ]
                                                                        ]
                                                                    ]
                                                            ]
                                                    ]
                                                ]
                                            ]

                                        ]
                                    ]
                                ))
                        ]
                        Html.div [
                            Html.button [
                                prop.text "Add include letter"
                                prop.className "p-3 bg-green-700 text-white font-bold rounded"
                                prop.onClick (fun _ ->
                                    let includeLetters = state.IncludeLetters @ [{ Letter = ""; InvalidPositions = [] }]
                                    setState { state with IncludeLetters = includeLetters })
                            ]

                        ]

                    ]
                ]
                Html.div [
                    Html.label [
                        Html.text "Filter: "
                    ]
                    Html.input [
                        prop.className "w-full rounded text-gray-800 p-2"
                        prop.onChange (fun (v: string) -> 
                            let upper = v.ToUpper()
                            setState({ state with Filter = upper}))
                    ]
                ]
                Html.div [
                    prop.children [
                        Html.h2 [
                            prop.className "font-bold text-2xl"
                            prop.text "Candidate words"
                        ]
                        Html.div [
                            prop.className "flex gap-4 flex-wrap justify-between"
                            prop.children
                                (filteredCandidates
                                |> List.take (min 100 filteredCandidates.Length)
                                |> List.map (fun w -> Html.span w))
                        ] 
                    ]
                ]
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