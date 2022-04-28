module App

open Elmish
open Elmish.React
open Feliz
open CmdExt
open Browser.Types
open Browser
open Fable.SimpleHttp

type State =
    { LoremIpsum: Deferred<Result<string, string>> }

type Msg = LoadLoremIpsum of AsyncOperationStatus<Result<string, string>>


let init () =
    { LoremIpsum = HasNotStartedYet }, Cmd.ofMsg (LoadLoremIpsum Started)

type Request =
    { url: string
      method: string
      body: string }

type Response = { statusCode: int; body: string }

let httpRequest (request: Request) : Async<Response> =
    Async.FromContinuations <| fun (resolve, reject, _) ->
        // create an instance
        let xhr = XMLHttpRequest.Create()
        // open the connection
        xhr.``open``(method=request.method, url=request.url)
        // setup the event handler that triggers when the content is loaded
        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = ReadyState.Done
            then
              // create the response
              let response = { statusCode = xhr.status; body = xhr.responseText }
              // transform response into a message
              resolve response

        // send the request
        xhr.send(request.body)

let update msg state =
    match msg with
    | LoadLoremIpsum Started ->
        let nextState = { state with LoremIpsum = InProgress }
        let loadLoremIpsum =
            async {
                let! (statusCode, responseText) = Http.get "/lorem-ipsum.txt"
                if statusCode = 200
                then return LoadLoremIpsum (Finished (Ok responseText))
                else return LoadLoremIpsum (Finished (Error "Could not load the content"))
            }

        nextState, Cmd.fromAsync loadLoremIpsum

    | LoadLoremIpsum (Finished result) ->
        let nextState = { state with LoremIpsum = Resolved result }
        nextState, Cmd.none



let render (state: State) (dispatch: Msg -> unit) =
    match state.LoremIpsum with
    | HasNotStartedYet -> Html.none

    | InProgress -> Html.div "Loading..."

    | Resolved (Ok content) ->
        Html.div [ prop.style [ style.color.green ]
                   prop.text content ]

    | Resolved (Error errorMsg) ->
        Html.div [ prop.style [ style.color.red ]
                   prop.text errorMsg ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
