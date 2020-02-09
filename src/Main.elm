module Main exposing (..)

import Browser
import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Todo =
  { id: Int
    , name: String
    , done: Bool
  }

type alias Model =
  { newTodoName : String
    , todoList: List Todo
  }

init : Model
init =
  { newTodoName = ""
    , todoList = [
      { id = 1
        , name = "Fazer café"
        , done = False
      }]
  }

-- UPDATE
type Msg
  = ChangeTodoInput String
    | AddTodo
    | ToggleTodo Int
    | RemoveTodo Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeTodoInput todoName ->
      { model | newTodoName = todoName }
    AddTodo ->
      if String.length model.newTodoName > 0 then
        { todoList = List.append model.todoList [{
          id = List.length model.todoList + 1
          , done = False
          , name = model.newTodoName
        }]
          , newTodoName = ""
        }
      else
        model
    ToggleTodo id ->
      { model | todoList = List.map (
        \todo ->
          if todo.id == id then
            { todo | done = not todo.done }
          else
            todo
      ) model.todoList
      }
    RemoveTodo id ->
      { model | todoList = List.filter (\todo -> todo.id /= id) model.todoList
      }


-- VIEW
view : Model -> Html Msg
view model =
  div [
    class "main-content"
    , style "font-family" "arial, sans-serif"
    , style "padding" "15px"
  ]
    [
      input [ type_ "text", placeholder "Todo name", value model.newTodoName, onInput ChangeTodoInput ] []
    , button [ onClick AddTodo ] [ text "Add Todo" ]
    , ul []
        (List.map (\item -> li [] [
          div [] [
            label [ classList [
              ("done", item.done)
            ]] [
              input [ type_ "checkbox", onClick <| ToggleTodo item.id ] []
              , text item.name
            ]
            , span [
                style "margin-left" "10px"
                , title "remove todo"
                , onClick <| RemoveTodo item.id
              ] [ text "x" ]
          ]
        ]) model.todoList)
    ]
