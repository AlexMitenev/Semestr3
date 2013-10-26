open System.Net
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Concurrent
open Microsoft.FSharp.Control.WebExtensions

let httpPat = "http://[a-z-A-Z0-9./_]*"
let imgPat  = "http://[a-z-A-Z0-9./_]*\.jpg"
let hrefPat = "href=\"/[a-z-A-Z0-9./_]*\""

let attendedLinks = new ConcurrentDictionary<string, bool>()
let downloadedImages = new ConcurrentDictionary<string, bool>()


let getHtml(url:string) =
    async { 
        try
                let uri       = new System.Uri(url)
                let webClient = new WebClient()
                let! html     = webClient.AsyncDownloadString(uri)
                return html
        with
        | e ->  printf "%s" e.Message
                return String.Empty
    }

let downloadImg(url:string) =
    async{
        try
            if not <| downloadedImages.ContainsKey(url) then 
                  downloadedImages.GetOrAdd(url, true) |> ignore
                  let fileName = string(url.GetHashCode()) + ".jpg"// using hash code for simple file name
                  let webClient = new WebClient()
                  webClient.DownloadFileAsync(Uri(url) ,fileName)
        with
        |error -> error.Message |> printf "%s"
    }

let getLinks (html : string) =
    async{
        try
              let hrefs = [ for m in Regex.Matches(html,hrefPat)  -> m.Value ]
              let linkStartPos = 6
              //delete "href= and last char(")
              let links = [ for h in hrefs -> h.[linkStartPos..(h.Length-2)]] 
              return links
        with
        |e -> printf "%A" e.Message
              return List.Empty
    }

let getImages (html : string) =
    async{
        try
               return [ for m in Regex.Matches(html, imgPat)  -> m.Value ]
        with
        | e -> printf "%A" e.Message
               return List.Empty
    } 

let rec crawle (url:string) = 
    async{
        attendedLinks.GetOrAdd(url, true) |> ignore 
        let! html  = getHtml url
        let! images = getImages html
        let! links  = getLinks html
        let notAttendedLink url = not <| attendedLinks.ContainsKey(url)

        images
            |> Seq.map downloadImg
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

        links
            |> List.map (fun x -> url + x)  //make full path
            |> List.filter notAttendedLink  //delete already attended links
            |> Seq.map crawle
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
    }

crawle "http://lenta.ru" |> Async.RunSynchronously