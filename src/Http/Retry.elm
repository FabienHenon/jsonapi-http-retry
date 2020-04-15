module Http.Retry exposing
    ( with
    , FailureCondition, onStatus, onUnauthenticatedStatus, onUnauthorizedStatus, onNetworkError, onTimeout, onAllFailures
    )

{-| Add retries to a jsonapi http task, based on a list of retry policies, until any one of
the policies fail too.

@docs with


# Failure conditions

@docs FailureCondition, onStatus, onUnauthenticatedStatus, onUnauthorizedStatus, onNetworkError, onTimeout, onAllFailures

-}

import Http
import Http.Error
import RemoteData
import Retry
import Task exposing (Task)


{-| FailureCondition contains a function that filter request errors
-}
type FailureCondition
    = FailureCondition (Http.Error.RequestError -> Bool)


{-| To use with `with` function. Filters errors to be retried by retrying errors base on the 401 status code
-}
onUnauthenticatedStatus : FailureCondition
onUnauthenticatedStatus =
    let
        conditionCheck err =
            case err of
                Http.Error.HttpError (Http.BadStatus 401) ->
                    True

                _ ->
                    False
    in
    FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying errors base on the 403 status code
-}
onUnauthorizedStatus : FailureCondition
onUnauthorizedStatus =
    let
        conditionCheck err =
            case err of
                Http.Error.HttpError (Http.BadStatus 403) ->
                    True

                _ ->
                    False
    in
    FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying errors based on the status code
-}
onStatus : Int -> FailureCondition
onStatus statusCode =
    let
        conditionCheck err =
            case err of
                Http.Error.HttpError (Http.BadStatus code) ->
                    code == statusCode

                _ ->
                    False
    in
    FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying all network errors
-}
onNetworkError : FailureCondition
onNetworkError =
    let
        conditionCheck err =
            case err of
                Http.Error.HttpError Http.NetworkError ->
                    True

                _ ->
                    False
    in
    FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying all timeout errors
-}
onTimeout : FailureCondition
onTimeout =
    let
        conditionCheck err =
            case err of
                Http.Error.HttpError Http.Timeout ->
                    True

                _ ->
                    False
    in
    FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying all errors
-}
onAllFailures : FailureCondition
onAllFailures =
    let
        conditionCheck err =
            True
    in
    FailureCondition conditionCheck


{-| Given a list of error handling `Policy` from [`Retry` package](https://package.elm-lang.org/packages/choonkeat/elm-retry/latest/) we can make our `originalTask`
retry on failure until any one of the `Policy` fails.

    originalTask
        |> Http.Retry.with
            [ Retry.maxDuration 7000
            , Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
            ]
            [ Http.Retry.onUnauthenticatedStatus ]
        |> Task.perform DidOriginalTask

-}
with : List (Retry.Policy Http.Error.RequestError) -> List FailureCondition -> Task Never (RemoteData.RemoteData Http.Error.RequestError data) -> Task Never (RemoteData.RemoteData Http.Error.RequestError data)
with errTasks failureConditions originalTask =
    originalTask
        |> Task.mapError (always (Http.Error.CustomError "no error"))
        |> Task.andThen (convertToError failureConditions)
        |> Retry.with errTasks
        |> Task.onError (RemoteData.Failure >> Task.succeed)


convertToError : List FailureCondition -> RemoteData.RemoteData Http.Error.RequestError data -> Task Http.Error.RequestError (RemoteData.RemoteData Http.Error.RequestError data)
convertToError failureConditions result =
    case result of
        RemoteData.Loading ->
            Task.succeed result

        RemoteData.NotAsked ->
            Task.succeed result

        RemoteData.Success d ->
            Task.succeed result

        RemoteData.Failure err ->
            if List.any (\(FailureCondition f) -> f err) failureConditions then
                Task.fail err

            else
                Task.succeed result
