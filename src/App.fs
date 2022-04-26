module App

open Elmish
open Elmish.React
open Feliz
open CmdExt

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

type State =
    { RandomNumber: Deferred<Result<double, string>> }

type Msg = GenerateRandomNumber of AsyncOperationStatus<Result<double, string>>

let init () =
    { RandomNumber = HasNotStartedYet }, Cmd.none

let rnd = System.Random()

let update msg state =
    match msg with
    | GenerateRandomNumber Started when state.RandomNumber = InProgress -> state, Cmd.none
    | GenerateRandomNumber Started ->
        let randomOp: Async<Msg> =
            async {
                do! Async.Sleep 1000
                let random = rnd.NextDouble()

                if random > 0.5 then
                    return GenerateRandomNumber(Finished(Ok random))
                else
                    let errorMsg =
                        sprintf "Failed! Random number %f was <= 0.5" random

                    return GenerateRandomNumber(Finished(Error errorMsg))
            }

        { state with RandomNumber = InProgress }, Cmd.fromAsync randomOp

    | GenerateRandomNumber (Finished (Ok randomNumber)) ->
        { state with RandomNumber = Resolved(Ok randomNumber) }, Cmd.none

    | GenerateRandomNumber (Finished (Error error)) -> { state with RandomNumber = Resolved(Error error) }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let content =
        match state.RandomNumber with
        | HasNotStartedYet -> Html.h1 "Hasn't started yet!"
        | InProgress -> Html.h1 "LOADING..."
        | Resolved (Ok number) ->
            Html.h1 [ prop.style [ style.color.green ]
                      prop.text (sprintf "Successfully generated random number: %f" number) ]
        | Resolved (Error errorMsg) ->
            Html.h1 [ prop.style [ style.color.crimson ]
                      prop.text errorMsg ]

    Html.div [ prop.children [ content
                               Html.button [ prop.disabled (state.RandomNumber = InProgress)
                                             prop.onClick (fun _ -> dispatch (GenerateRandomNumber Started))
                                             prop.text "Generate Random" ] ] ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
