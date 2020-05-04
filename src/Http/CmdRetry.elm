module Http.CmdRetry exposing
    ( with, newAttempt
    , RetryContext
    )

{-| Tries to perform a `Task` and retry it upon failure, allowing you to execute other `Cmd`s between each failure.

The following example executes a request with a retry policy when the request fails and
executes a port before each new retry:

    type Msg
        = OnRetry (Http.CmdRetry.RetryContext Msg Entity)
        | OnEntityRetrieved (RemoteData.RemoteData Http.Error.RequestError Entity)

    type alias Model =
        { entity : RemoteData.RemoteData Http.Error.RequestError Entity }

    init : ( Model, Cmd Msg )
    init =
        ( { entity = RemoteData.Loading }
        , getEntity
            |> Http.CmdRetry.with
                [ Http.Retry.maxDuration 7000
                , Http.Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
                ]
                [ Http.Retry.onUnauthenticatedStatus ]
                OnRetry
        )

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            OnRetry retryContext ->
                ( model
                , Http.CmdRetry.newAttempt
                    executePort
                    OnEntityRetrieved
                    retryContext
                )

            OnEntityRetrieved entity ->
                ( { model | entity = entity }, Cmd.none )

    getEntity : Task Never (RemoteData.RemoteData Http.Error.RequestError Entity)
    getEntity =
        Request.request
            { url = "<http://endpoint">
            , headers = []
            , body = Json.Encode.object []
            , documentDecoder = JsonApi.Decode.resources "resource-type" entityDecoder
            }


# Retry

@docs with, newAttempt


# Types

@docs RetryContext

-}

import Http.Error
import Http.Internal as Internal
import RemoteData
import Task exposing (Task)
import Time


{-| Type used by the module to keep the context of the retry process.
You will never handle it directly

`msg` is your `Msg` type and `data` is the data you you to receive from your request.

-}
type RetryContext msg data
    = FailedTask (Context msg data)
    | FinishedTask (Task Never (RemoteData.RemoteData Http.Error.RequestError data))


type alias Context msg data =
    { partContext : PartContext msg data
    , lastError : Http.Error.RequestError
    }


type alias PartContext msg data =
    { startTime : Int
    , failureConditions : List Internal.FailureCondition
    , originalTask : Task Never (RemoteData.RemoteData Http.Error.RequestError data)
    , onRetryMsg : RetryContext msg data -> msg
    , policies : List (Internal.Policy Http.Error.RequestError)
    }


{-| Attempt a new retry from your update function with the `RetryContext` you received.
The first parameter allows you to execute a `Cmd` just before the next retry.
The second parameter is the message you want to send when the request finally succeeded or failed (after all configured retries)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            OnRetry retryContext ->
                ( model
                , Http.CmdRetry.newAttempt
                    (\lastError ->
                        doSomethingWithLastError lastError
                    )
                    OnRequestDone
                    retryContext
                )

-}
newAttempt :
    (Http.Error.RequestError -> Cmd msg)
    -> (RemoteData.RemoteData Http.Error.RequestError data -> msg)
    -> RetryContext msg data
    -> Cmd msg
newAttempt onRetryCmd msg retryContext =
    case retryContext of
        FailedTask ({ partContext, lastError } as context) ->
            Cmd.batch
                [ onRetryCmd lastError
                , retryFromContext partContext
                    |> Task.perform partContext.onRetryMsg
                ]

        FinishedTask task ->
            Task.perform msg task


{-| Tries to execute the given task. You will receive a message with the retry context.
From there you will call `newAttempt` which will handle your request retry, allowing you
to execute a `Cmd` before the next retry.

    originalTask
        |> Http.CmdRetry.with
            [ Http.Retry.maxDuration 7000
            , Http.Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
            ]
            [ Http.Retry.onUnauthenticatedStatus ]
            OnRetry

-}
with :
    List (Internal.Policy Http.Error.RequestError)
    -> List Internal.FailureCondition
    -> (RetryContext msg data -> msg)
    -> Task Never (RemoteData.RemoteData Http.Error.RequestError data)
    -> Cmd msg
with errTasks failureConditions onRetryMsg originalTask =
    Task.map Time.posixToMillis Time.now
        |> Task.map
            (\nowMillis ->
                { policies = errTasks
                , startTime = nowMillis
                , failureConditions = failureConditions
                , onRetryMsg = onRetryMsg
                , originalTask = originalTask
                }
            )
        |> Task.andThen retryFromContext
        |> Task.perform onRetryMsg


retryFromContext : PartContext msg data -> Task Never (RetryContext msg data)
retryFromContext { startTime, policies, originalTask, onRetryMsg, failureConditions } =
    let
        onError time currPolicies err =
            currPolicies
                |> List.map (\((Internal.Policy nextPolicy) as cfg) -> nextPolicy time cfg err)
                |> Task.sequence
                |> Task.map
                    (\nextPolicies ->
                        FailedTask
                            { partContext =
                                { startTime = time
                                , failureConditions = failureConditions
                                , originalTask = originalTask
                                , onRetryMsg = onRetryMsg
                                , policies = nextPolicies
                                }
                            , lastError = err
                            }
                    )
                |> Task.onError (RemoteData.Failure >> Task.succeed >> FinishedTask >> Task.succeed)
    in
    originalTask
        |> Task.mapError (always (Http.Error.CustomError "no error"))
        |> Task.andThen (Internal.convertToError failureConditions)
        |> Task.map (Task.succeed >> FinishedTask)
        |> Task.onError (onError startTime policies)
