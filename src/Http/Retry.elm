module Http.Retry exposing
    ( with
    , Policy, maxRetries, maxDuration, constantInterval, exponentialBackoff
    , FailureCondition, onStatus, onUnauthenticatedStatus, onUnauthorizedStatus, onNetworkError, onTimeout, onAllFailures
    )

{-| Add retries to a jsonapi http task, based on a list of retry policies, until any one of
the policies fail too.

@docs with


# Policies

@docs Policy, maxRetries, maxDuration, constantInterval, exponentialBackoff


# Failure conditions

@docs FailureCondition, onStatus, onUnauthenticatedStatus, onUnauthorizedStatus, onNetworkError, onTimeout, onAllFailures

-}

import Http
import Http.Error
import Http.Internal as Internal
import Process
import Random
import RemoteData
import Task exposing (Task)
import Time


{-| FailureCondition contains a function that filter request errors
-}
type alias FailureCondition =
    Internal.FailureCondition


{-| A [`Policy`](#Policy) is attached with a function that will return another
[`Policy`](#Policy) as a [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#Task) value.
The arguments of the function are

  - `Int` timestamp of when we first started `originalTask`, in milliseconds
  - `Policy x` the current policy; destructure to obtain the function to call
  - `x` last error from attempting `originalTask`
    Refer to [`maxRetries`](#maxRetries) source code for a simple example.

-}
type alias Policy x =
    Internal.Policy x


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
    Internal.FailureCondition conditionCheck


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
    Internal.FailureCondition conditionCheck


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
    Internal.FailureCondition conditionCheck


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
    Internal.FailureCondition conditionCheck


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
    Internal.FailureCondition conditionCheck


{-| To use with `with` function. Filters errors to be retried by retrying all errors
-}
onAllFailures : FailureCondition
onAllFailures =
    let
        conditionCheck err =
            True
    in
    Internal.FailureCondition conditionCheck


{-| Given a list of error handling `Policy` we can make our `originalTask`
retry on failure until any one of the `Policy` fails or the request succeeds.

    originalTask
        |> Http.Retry.with
            [ Http.Retry.maxDuration 7000
            , Http.Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
            ]
            [ Http.Retry.onUnauthenticatedStatus ]
        |> Task.perform DidOriginalTask

-}
with : List (Policy Http.Error.RequestError) -> List FailureCondition -> Task Never (RemoteData.RemoteData Http.Error.RequestError data) -> Task Never (RemoteData.RemoteData Http.Error.RequestError data)
with errTasks failureConditions originalTask =
    originalTask
        |> Task.mapError (always (Http.Error.CustomError "no error"))
        |> Task.andThen (Internal.convertToError failureConditions)
        |> withInternal errTasks
        |> Task.onError (RemoteData.Failure >> Task.succeed)


withInternal : List (Policy x) -> Task x a -> Task x a
withInternal errTasks originalTask =
    let
        onError startTime currPolicies err =
            currPolicies
                |> List.map (\((Internal.Policy nextPolicy) as cfg) -> nextPolicy startTime cfg err)
                |> Task.sequence
                |> Task.andThen (\nextPolicies -> Task.onError (onError startTime nextPolicies) originalTask)
    in
    Task.map Time.posixToMillis Time.now
        |> Task.andThen
            (\nowMillis -> Task.onError (onError nowMillis errTasks) originalTask)


{-| Stop retrying `originalTask` after a number of retries.

    Retry.with [ Retry.maxRetries 20 ] [] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above does NOT sleep between retries; best to combine with
[`constantInterval`](#constantInterval) or [`exponentialBackoff`](#exponentialBackoff)

-}
maxRetries : Int -> Policy x
maxRetries int =
    let
        nextPolicy _ _ err =
            if int <= 0 then
                Task.fail err

            else
                Task.succeed (maxRetries (int - 1))
    in
    Internal.Policy nextPolicy


{-| Stop retrying `originalTask` after some number of milliseconds.

    Retry.with [ Retry.maxDuration 7000 ] [] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above does NOT sleep between retries; best to combine with
[`constantInterval`](#constantInterval) or [`exponentialBackoff`](#exponentialBackoff)

-}
maxDuration : Int -> Policy x
maxDuration duration =
    let
        nextPolicy startTime sameTask err =
            Task.map Time.posixToMillis Time.now
                |> Task.andThen
                    (\now ->
                        if now - startTime >= duration then
                            Task.fail err

                        else
                            Task.succeed sameTask
                    )
    in
    Internal.Policy nextPolicy


{-| Sleep for the same number of milliseconds before every retry.

    Retry.with [ Retry.constantInterval 1000 ] [] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above will keep retrying `originalTask`; best to combine with
[`maxRetries`](#maxRetries) or [`maxDuration`](#maxDuration)

-}
constantInterval : Float -> Policy x
constantInterval duration =
    let
        nextPolicy _ sameTask _ =
            Process.sleep duration
                |> Task.andThen (\_ -> Task.succeed sameTask)
    in
    Internal.Policy nextPolicy


{-| Sleep for an increasing number of milliseconds before every retry. Backoff
algorithim is based off [https://github.com/cenkalti/backoff](https://github.com/cenkalti/backoff/blob/4b4cebaf850ec58f1bb1fec5bdebdf8501c2bc3f/exponential.go#L144-L153)

    Retry.with [ Retry.exponentialBackoff { interval = 500, maxInterval = 3000 } ] [] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above will keep retrying `originalTask`; best to combine with
[`maxRetries`](#maxRetries) or [`maxDuration`](#maxDuration)

-}
exponentialBackoff : { interval : Float, maxInterval : Float } -> Policy x
exponentialBackoff { interval, maxInterval } =
    let
        backoffWith seed currInterval =
            let
                ( calcInterval, nextSeed ) =
                    Random.step
                        (nextIntervalGenerator { randomizationFactor = 0.5, multiplier = 1.5, interval = currInterval })
                        seed

                nextPolicy _ _ err =
                    Process.sleep currInterval
                        |> Task.andThen (\_ -> Task.succeed (backoffWith nextSeed (min calcInterval maxInterval)))
            in
            Internal.Policy nextPolicy
    in
    backoffWith (Random.initialSeed 0) interval


nextIntervalGenerator : { randomizationFactor : Float, multiplier : Float, interval : Float } -> Random.Generator Float
nextIntervalGenerator { randomizationFactor, multiplier, interval } =
    let
        minInterval =
            interval * randomizationFactor

        maxInterval =
            interval * (1 + randomizationFactor)
    in
    Random.float 0 1
        |> Random.map (\randf -> multiplier * (minInterval + (randf * (maxInterval - minInterval + 1))))
