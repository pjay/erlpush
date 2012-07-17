-module(erlpush_push_controller, [Req, SessionID]).
-compile(export_all).

broadcast('GET', [AppId], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case boss_db:find(AppId) of
        undefined ->
            not_found;
        App ->
            case App:push_user_id() of
                UserId -> {ok, [{user_id, UserId}, {app, App}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end;
broadcast('POST', [AppId], ExtraInfo) ->
    App = boss_db:find(AppId),
    push_dispatcher:start(),
    case Req:post_param("type") of
        "ios" ->
            push_dispatcher:send_broadcast_ios(App, broadcast_payload_ios()),
            boss_flash:add(SessionID, notice, "The broadcast notification is being sent", ""),
            {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]};
        "gcm" ->
            push_dispatcher:send_broadcast_gcm(App, broadcast_payload_gcm()),
            boss_flash:add(SessionID, notice, "The broadcast notification is being sent", ""),
            {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]};
        _ ->
            boss_flash:add(SessionID, error, "Bad request", "the 'type' field doesn't contain a valid value"),
            {redirect, [{controller, "push"}, {action, "broadcast"}, {id, AppId}]}
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).

%% @todo Data validation
broadcast_payload_ios() ->
    Badge = string:strip(Req:post_param("badge")),
    BadgeInt = case string:to_integer(Badge) of
        {Int, Rest} when is_integer(Int) -> Int;
        {error, _Reason} -> undefined
    end,
    AlertBin = case string:strip(Req:post_param("alert")) of
        Alert when length(Alert) > 0 -> list_to_binary(Alert);
        _ -> undefined
    end,
    SoundBin = case string:strip(Req:post_param("sound")) of
        Sound when length(Sound) > 0 -> list_to_binary(Sound);
        _ -> undefined
    end,
    RawAPSPayload = [{<<"badge">>, BadgeInt}, {<<"alert">>, AlertBin}, {<<"sound">>, SoundBin}],
    APSPayload = lists:filter(fun({Key, Value}) -> Value =/= undefined end, RawAPSPayload),
    [{<<"aps">>, APSPayload}].

%% @todo Data validation
broadcast_payload_gcm() ->
    Key = string:strip(Req:post_param("extra_key")),
    Value = string:strip(Req:post_param("extra_value")),
    [{list_to_binary(Key),list_to_binary(Value)}].