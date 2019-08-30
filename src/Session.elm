module Session exposing (..)

import Api exposing (..)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time
import Http exposing (..)

-- TYPES

type Session
    = LoggedIn Nav.Key Cred
    |  Guest Nav.Key


-- INFO


viewer : Session -> Maybe Cred
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (val)

        Guest _ ->
            Nothing
            
check c = 
    case c of
        Just ok ->
            ok
    
        Nothing ->
            True


changeInterCeptor error sessionHere=
    case error of
        Just err ->
            if err == "401" then
                case sessionHere of
                    LoggedIn _ val ->
                        Api.refreshFetchData ()
                
                    Guest _ ->
                       Cmd.none
                            
            else
                Cmd.none
    
        Nothing ->
                Cmd.none


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key
        Guest key ->
            key


changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Api.credDecoder

fromViewer : Nav.Key -> Maybe Cred -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
    