-module(erlpush_events_controller, [Req, SessionID]).

-export([index/3, before_/1]).

-define(PER_PAGE, 25).

index('GET', [AppId, Skip], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case boss_db:find(AppId) of
        undefined ->
            not_found;
        App ->
            case App:push_user_id() of
                UserId ->
                    ValidatedSkip = safe_string_to_integer(Skip),
                    EventsPlusOne = boss_db:find(event, [app_id = App:id()], ?PER_PAGE + 1, ValidatedSkip, creation_time, num_descending),
                    Events = lists:sublist(EventsPlusOne, ?PER_PAGE),
                    SkipPrevious = max(ValidatedSkip - ?PER_PAGE, 0),
                    HasNextPage = length(EventsPlusOne) > ?PER_PAGE,
                    SkipNext = ValidatedSkip + ?PER_PAGE,
                    {ok, [{app, App}, {events, Events}, {has_next_page, HasNextPage}, {skip, ValidatedSkip},
                          {skip_previous, SkipPrevious}, {skip_next, SkipNext}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).

safe_string_to_integer(String) ->
    case string:to_integer(String) of
        {error, _Reason} -> 0;
        {Int, _Rest} -> Int
    end.