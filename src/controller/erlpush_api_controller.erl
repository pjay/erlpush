-module(erlpush_api_controller, [Req]).

-export([device_tokens/2, registrations/2, push/2]).

device_tokens('PUT', [TokenValue]) ->
    put_device_token_or_registration(device_token, TokenValue);
device_tokens('DELETE', [TokenValue]) ->
    delete_device_token_or_registration(device_token, TokenValue).

registrations('PUT', [RegistrationId]) ->
    put_device_token_or_registration(registration, RegistrationId);
registrations('DELETE', [RegistrationId]) ->
    delete_device_token_or_registration(registration, RegistrationId).

push('POST', ["broadcast"]) ->
    case proplists:get_value(app, app_with_master_secret()) of
        undefined -> result_invalid_key_or_secret();
        App -> send_broadcast(App)
    end.

app_with_api_secret() ->
    app_with_secret_type(api_secret).

app_with_master_secret() ->
    app_with_secret_type(master_secret).

app_with_secret_type(SecretType) ->
    case Req:header("Authorization") of
        undefined -> [];
        AuthHeader ->
            [AuthType, EncodedAuth] = string:tokens(AuthHeader, " "),
            DecodedAuth = base64:decode_to_string(EncodedAuth),
            [AppKey, AppSecret] = string:tokens(DecodedAuth, ":"),
            Results = boss_db:find(app, [api_key = AppKey, SecretType = AppSecret]),
            case length(Results) of
                1 -> [{app, hd(Results)}];
                _ -> []
            end
    end.

result_invalid_key_or_secret() ->
    {404, "Invalid application key or secret", [{"Content-Type", "text/plain"}]}.

result_bad_request_with_message(Message) ->
    {400, Message, [{"Content-Type", "text/plain"}]}.

put_device_token_or_registration(Type, Value) ->
    case proplists:get_value(app, app_with_api_secret()) of
        undefined -> result_invalid_key_or_secret();
        App ->
            case boss_db:find(Type, [value = Value]) of
                Records when length(Records) > 0 ->
                    Record = hd(Records),
                    NewRecord = Record:set(last_registration_time, calendar:universal_time()),
                    StatusCode = 200;
                _ ->
                    NewRecord = boss_record:new(Type, [{app_id, App:id()}, {value, Value}, {last_registration_time, calendar:universal_time()}]),
                    StatusCode = 201
            end,
            case NewRecord:save() of
                {ok, SavedRecord} ->
                    {StatusCode, "", []};
                {error, ErrorList} ->
                    {400, hd(ErrorList), [{"Content-Type", "text/plain"}]}
            end
    end.

delete_device_token_or_registration(Type, Value) ->
    case proplists:get_value(app, app_with_api_secret()) of
        undefined -> result_invalid_key_or_secret();
        App ->
            case boss_db:find(Type, [value = Value]) of
                Records when length(Records) > 0 ->
                    Record = hd(Records),
                    case boss_db:delete(Record:id()) of
                        ok -> {204, "", []};
                        {error, Reason} ->
                            error_logger:error_msg("Cannot delete device token or registration from the database with reason ~p~n", [Reason]),
                            {error, "", []}
                    end;
                _ ->
                    not_found
            end
    end.

send_broadcast(App) ->
    case Req:header(content_type) of
        "application/json" ->
            case Req:request_body() of
                Binary when is_binary(Binary) ->
                    case jsx:is_json(Binary) of
                        true ->
                            Json = jsx:json_to_term(Binary),
                            push_dispatcher:start(),
                            push_dispatcher:send_broadcast(App, Json),
                            {200, "", []};
                        false -> result_bad_request_with_message("Invalid JSON")
                    end;
                _ -> result_bad_request_with_message("Invalid content")
            end;
        undefined -> result_bad_request_with_message("Invalid content type")
    end.