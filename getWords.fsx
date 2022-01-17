open System.Net.Http
open System.IO
open System.Net.Http.Headers
open System.Net

let getWebString (url: string) =
    async {
        use client = new HttpClient(
            new HttpClientHandler(
                AutomaticDecompression = DecompressionMethods.All
            ))
        client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("text/html"))

        let! response = 
            client.GetAsync(url)
            |> Async.AwaitTask
        let! content = 
            response.Content.ReadAsStringAsync()
            |> Async.AwaitTask
        return content
    }

open System.Text.RegularExpressions
let extractWords (page: string) =
    let rex = new Regex("<span class=mot[2]?>(.*?)</span>")
    let matches = rex.Matches(page)
    let strings =
        matches
        |> Seq.map (fun mc -> mc.Groups[1].Value)
        |> List.ofSeq
    (String.concat "" strings).Split(" ") |> List.ofArray

let content = 
    getWebString "https://www.bestwordlist.com/5letterwords.htm"
//    getWebString "https://www.vg.no"
    |> Async.RunSynchronously

let getWordsForSite url = 
    async {
        let! content = getWebString url
        return extractWords content
    }

let urls = 
    [for i in 1..15 do if i = 1 then yield "" else yield sprintf "page%i" i]
    |> List.map (fun i -> sprintf "https://www.bestwordlist.com/5letterwords%s.htm" i)

let words =
    urls
    |> List.map (fun url -> getWordsForSite url)
    |> List.map (fun task -> Async.RunSynchronously task)
    |> List.ofSeq
    |> List.concat
    |> List.filter (fun w ->
        if w = "black"
        then 
            printfn "found black: %A" (w.ToUpper() = w, w.ToUpper(), w)
        w.Length = 5 && 
        (not (w.Contains('<')) &&
        w.ToUpper() = w))
    |> List.distinct
    |> List.map(fun w ->
        if w.Contains("black")
        then 
            printfn "==> found black: %A" (w.ToUpper() = w, w.ToUpper(), w)
        w
    )

System.IO.File.WriteAllText("words.json", System.Text.Json.JsonSerializer.Serialize(words))

//System.IO.FileInfo.(System.Text.Json.JsonSerializer.Serialize(words), "words.json")