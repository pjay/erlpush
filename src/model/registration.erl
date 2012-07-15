-module(registration, [Id, AppId::string(), Value::string(), LastRegistrationTime::datetime()]).
-compile(export_all).

-belongs_to(app).
-has({notifications, many}).

validation_tests() ->
	[{fun() -> length(Value) > 0 end, "Registration value is mandatory"}].