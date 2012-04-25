-module(erlpush_api_controller, [Req]).
-compile(export_all).

device_tokens('PUT', [TokenValue], ExtraInfo) ->
    case proplists:get_value(mobile_application, ExtraInfo) of
        undefined ->
            not_found;
        App ->
            NewToken = boss_record:new(device_token, [{mobile_application_id, App:id()}, {value, TokenValue}, {last_registration_time, calendar:universal_time()}]),
            case NewToken:save() of
                {ok, SavedToken} ->
                    {output, "OK"};
                {error, ErrorList} ->
                    error_logger:info_report(ErrorList),
                    {output, "ERROR"}
            end
    end;
device_tokens('DELETE', [TokenValue], ExtraInfo) ->
    ok.

before_(_ActionName) ->
    AuthHeader = Req:header("Authorization"),
    [AuthType, EncodedAuth] = string:tokens(AuthHeader, " "),
    DecodedAuth = base64:decode_to_string(EncodedAuth),
    [AppKey, AppSecret] = string:tokens(DecodedAuth, ":"),
    Results = boss_db:find(mobile_application, [api_key = AppKey, api_secret = AppSecret]),
    case length(Results) of
        1 ->
            App = hd(Results),
            {ok, [{mobile_application, App}]};
        _ ->
            {ok, []}
    end.