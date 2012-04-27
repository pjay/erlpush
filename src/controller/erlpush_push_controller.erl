-module(erlpush_push_controller, [Req, SessionID]).
-compile(export_all).

broadcast('GET', [AppId], ExtraInfo) ->
	ok;
broadcast('POST', [AppId], ExtraInfo) ->
	%% TODO
	boss_flash:add(SessionID, notice, "Message sent", ""),
	{redirect, [{action, "broadcast", app_id, "asdf"}]}.