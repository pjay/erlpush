-module(erlpush_api_controller, [Req]).
-compile(export_all).

device_tokens('PUT', [TokenValue], ExtraInfo) ->
    case proplists:get_value(mobile_application, ExtraInfo) of
        undefined ->
            result_invalid_key_or_secret();
        App ->
            case boss_db:find(device_token, [value = TokenValue]) of
                Tokens when length(Tokens) > 0 ->
                    Token = hd(Tokens),
                    NewToken = Token:set(last_registration_time, calendar:universal_time()),
                    StatusCode = 200;
                _ ->
                    NewToken = boss_record:new(device_token, [{mobile_application_id, App:id()}, {value, TokenValue}, {last_registration_time, calendar:universal_time()}]),
                    StatusCode = 201
            end,
            case NewToken:save() of
                {ok, SavedToken} ->
                    {StatusCode, "", []};
                {error, ErrorList} ->
                    {400, hd(ErrorList), [{"Content-Type", "text/plain"}]}
            end
    end;
device_tokens('DELETE', [TokenValue], ExtraInfo) ->
    case proplists:get_value(mobile_application, ExtraInfo) of
        undefined ->
            result_invalid_key_or_secret();
        App ->
            case boss_db:find(device_token, [value = TokenValue]) of
                Tokens when length(Tokens) > 0 ->
                    Token = hd(Tokens),
                    boss_db:delete(Token:id()),
                    {204, "", []};
                _ ->
                    not_found
            end
    end.

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

result_invalid_key_or_secret() ->
    {404, "Invalid application key or secret", [{"Content-Type", "text/plain"}]}.