-module(erlpush_device_tokens_controller, [Req, SessionID]).
-compile(export_all).

index('GET', [AppId], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case boss_db:find(AppId) of
        undefined ->
            not_found;
        {error, Reason} ->
            error_logger:error_msg("Error finding app with reason: ~p~n", [Reason]),
            {error, "Error finding app", []};
        App ->
            case App:push_user_id() of
                UserId ->
                    DeviceTokens = boss_db:find(device_token, [app_id = AppId], 50, 0, last_registration_time, num_descending),
                    {ok, [{app, App}, {device_tokens, DeviceTokens}]};
                _ -> not_found
            end
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).