module App

open Elmish
open Elmish.React
open Feliz
open CmdExt
open Browser.Types
open Browser
open Fable.SimpleHttp

type HackernewsItem =
    { id: int
      title: string
      url: string option }

type State =
    { StoryItems: Deferred<Result<HackernewsItem list, string>> }

type Msg = LoadStoryItems of AsyncOperationStatus<Result<HackernewsItem list, string>>


let init () =
    let initialState = { StoryItems = HasNotStartedYet }
    let initialCmd = Cmd.ofMsg (LoadStoryItems Started)
    initialState, initialCmd

let loadStoryItems = async {
    // simulate network delay
    do! Async.Sleep 1500
    let storyItems = [ { id = 1; title = "Example title"; url = None } ]
    return LoadStoryItems (Finished (Ok storyItems))
}

let update (msg: Msg) (state: State) =
    match msg with
    | LoadStoryItems Started ->
        let nextState = { state with StoryItems = InProgress }
        nextState, Cmd.fromAsync loadStoryItems

    | LoadStoryItems (Finished (Ok storyItems)) ->
        let nextState =
            { state with StoryItems = Resolved(Ok storyItems) }

        nextState, Cmd.none

    | LoadStoryItems (Finished (Error error)) ->
        let nextState =
            { state with StoryItems = Resolved(Error error) }

        nextState, Cmd.none



type Request =
    { url: string
      method: string
      body: string }

type Response = { statusCode: int; body: string }

let httpRequest (request: Request) : Async<Response> =
    Async.FromContinuations
    <| fun (resolve, reject, _) ->
        // create an instance
        let xhr = XMLHttpRequest.Create()
        // open the connection
        xhr.``open`` (method = request.method, url = request.url)
        // setup the event handler that triggers when the content is loaded
        xhr.onreadystatechange <-
            fun _ ->
                if xhr.readyState = ReadyState.Done then
                    // create the response
                    let response =
                        { statusCode = xhr.status
                          body = xhr.responseText }
                    // transform response into a message
                    resolve response

        // send the request
        xhr.send (request.body)

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let renderItem item =
  Html.div [
    prop.key item.id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      match item.url with
      | Some url ->
          Html.a [
            prop.style [ style.textDecoration.underline ]
            prop.target.blank
            prop.href url
            prop.text item.title
          ]
      | None ->
          Html.p item.title
    ]
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> React.fragment [ for item in items -> renderItem item ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      renderItems state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
