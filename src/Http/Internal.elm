module Http.Internal exposing (FailureCondition(..), Policy(..), convertToError)

import Http.Error
import RemoteData
import Task exposing (Task)


type FailureCondition
    = FailureCondition (Http.Error.RequestError -> Bool)


type Policy x
    = Policy (Int -> Policy x -> x -> Task x (Policy x))


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
