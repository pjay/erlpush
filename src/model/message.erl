-module(message, [Id, AppId::string(), RegistrationId::string(), Payload::binary(), CreationTime::datetime(), DeliveryTime::datetime()]).
-compile(export_all).

-belongs_to(app).
-belongs_to(registration).

before_create() ->
    ModifiedRecord = set([{creation_time, calendar:universal_time()}]),
    {ok, ModifiedRecord}.