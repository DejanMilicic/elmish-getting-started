module App

open Elmish
open Elmish.React
open Feliz
open System

[<Literal>]
let ENTER_KEY = 13.

type Todo =
    {
        Id: Guid
        Description: string
        Completed: bool
    }

type TodoBeingEdited = { Id: Guid; Description: string }

type Filter =
    | All
    | Completed
    | NotCompleted

type State =
    {
        NewTodo: string
        TodoList: Todo list
        TodoBeingEdited: TodoBeingEdited option
        TodoFilter: Filter
    }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string
    | SetActiveFilterTab of Filter

let init () =
    {
        NewTodo = ""
        TodoList =
            [
                {
                    Id = Guid.NewGuid()
                    Description = "Learn F#"
                    Completed = false
                }
            ]
        TodoBeingEdited = None
        TodoFilter = Filter.All
    }

let update (msg: Msg) (state: State) : State =
    match msg with
    | SetActiveFilterTab filter -> { state with TodoFilter = filter }

    | SetNewTodo desc -> { state with NewTodo = desc }

    | ToggleCompleted todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId then
                    { todo with
                        Completed = not todo.Completed
                    }
                else
                    todo)

        { state with TodoList = nextTodoList }

    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

    | AddNewTodo when state.NewTodo = "" -> state

    | AddNewTodo ->
        let nextTodo =
            {
                Id = Guid.NewGuid()
                Description = state.NewTodo
                Completed = false
            }

        { state with
            NewTodo = ""
            TodoList = List.append state.TodoList [ nextTodo ]
        }

    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo ->
                {
                    Id = todoId
                    Description = todo.Description
                })

        { state with
            TodoBeingEdited = nextEditModel
        }

    | CancelEdit -> { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
                state.TodoList
                |> List.map (fun todo ->
                    if todo.Id = todoBeingEdited.Id then
                        { todo with
                            Description = todoBeingEdited.Description
                        }
                    else
                        todo)

            { state with
                TodoList = nextTodoList
                TodoBeingEdited = None
            }

    | SetEditedDescription newText ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map (fun todoBeingEdited ->
                { todoBeingEdited with
                    Description = newText
                })

        { state with
            TodoBeingEdited = nextEditModel
        }

let appTitle =
    Html.p [
        prop.className "title"
        prop.text "Elmish todo list"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [ "field"; "has-addons" ]
        prop.children [
            Html.div [
                prop.classes [
                    "control"
                    "is-expanded"
                ]
                prop.children [
                    Html.input [
                        prop.classes [ "input"; "is-medium" ]
                        prop.valueOrDefault state.NewTodo
                        prop.onChange (SetNewTodo >> dispatch)
                        prop.onKeyDown (fun ev ->
                            match ev with
                            | ev when ev.keyCode = ENTER_KEY -> dispatch AddNewTodo
                            | _ -> ())
                    ]
                ]
            ]

            Html.div [
                prop.className "control"
                prop.children [
                    Html.button [
                        prop.classes [
                            "button"
                            "is-primary"
                            "is-medium"
                        ]
                        prop.onClick (fun _ -> dispatch AddNewTodo)
                        prop.children [
                            Html.i [
                                prop.classes [ "fa"; "fa-plus" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderTodo (state: State) (todo: Todo) (dispatch: Msg -> unit) =
    div [
            "box"
            match state.TodoFilter with
            | Completed ->
                if not todo.Completed then
                    "is-hidden"
                else
                    ""
            | NotCompleted ->
                if todo.Completed then
                    "is-hidden"
                else
                    ""
            | All -> ""
        ] [
        div [
                "columns"
                "is-mobile"
                "is-vcentered"
            ] [
            div [ "column" ] [
                Html.p [
                    prop.className "subtitle"
                    prop.text todo.Description
                ]
            ]

            div [ "column"; "is-narrow" ] [
                div [ "buttons" ] [
                    Html.button [
                        prop.classes [
                            "button"
                            if todo.Completed then "is-success"
                        ]
                        prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                        prop.children [
                            Html.i [
                                prop.classes [ "fa"; "fa-check" ]
                            ]
                        ]
                    ]

                    Html.button [
                        prop.classes [ "button"; "is-primary" ]
                        prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                        prop.children [
                            Html.i [
                                prop.classes [ "fa"; "fa-edit" ]
                            ]
                        ]
                    ]

                    Html.button [
                        prop.classes [ "button"; "is-danger" ]
                        prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                        prop.children [
                            Html.i [
                                prop.classes [ "fa"; "fa-times" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderEditForm (todoBeingEdited: TodoBeingEdited) (todo: Todo) (dispatch: Msg -> unit) =
    div [ "box" ] [
        div [ "field"; "is-grouped" ] [
            div [ "control"; "is-expanded" ] [
                Html.input [
                    prop.classes [ "input"; "is-medium" ]
                    prop.valueOrDefault todoBeingEdited.Description
                    prop.onTextChange (SetEditedDescription >> dispatch)
                    prop.onKeyDown (fun ev ->
                        match ev with
                        | ev when ev.keyCode = ENTER_KEY -> dispatch ApplyEdit
                        | _ -> ())
                ]
            ]

            div [ "control"; "buttons" ] [
                Html.button [
                    prop.classes [ 
                        "button"; 
                        if todoBeingEdited.Description = todo.Description then "is-outlined" else "is-primary"
                    ]
                    prop.onClick (fun _ -> dispatch ApplyEdit)
                    prop.children [
                        Html.i [
                            prop.classes [ "fa"; "fa-save" ]
                        ]
                    ]
                ]

                Html.button [
                    prop.classes [ "button"; "is-warning" ]
                    prop.onClick (fun _ -> dispatch CancelEdit)
                    prop.children [
                        Html.i [
                            prop.classes [ "fa"; "fa-arrow-right" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let todoList (state: State) (dispatch: Msg -> unit) =
    Html.ul [
        prop.children [
            for todo in state.TodoList ->
                match state.TodoBeingEdited with
                | Some todoBeingEdited when todoBeingEdited.Id = todo.Id -> renderEditForm todoBeingEdited todo dispatch
                | otherwise -> renderTodo state todo dispatch
        ]
    ]

let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
    div [ "tabs"; "is-toggle"; "is-fullwidth" ] [
        Html.ul [
            Html.li [
                prop.className [
                    if state.TodoFilter = All then
                        "is-active"
                ]
                prop.children [
                    Html.a [
                        prop.text "All"
                        prop.onClick (fun _ -> dispatch (SetActiveFilterTab All))
                    ]
                ]
            ]

            Html.li [
                prop.className [
                    if state.TodoFilter = Completed then
                        "is-active"
                ]
                prop.children [
                    Html.a [
                        prop.text "Completed"
                        prop.onClick (fun _ -> dispatch (SetActiveFilterTab Completed))
                    ]
                ]
            ]

            Html.li [
                prop.className [
                    if state.TodoFilter = NotCompleted then
                        "is-active"
                ]
                prop.children [
                    Html.a [
                        prop.text "Not Completed"
                        prop.onClick (fun _ -> dispatch (SetActiveFilterTab NotCompleted))
                    ]
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [
            appTitle
            inputField state dispatch
            renderFilterTabs state dispatch
            todoList state dispatch
        ]
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
