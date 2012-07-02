-module(notification, [Id, AppId::string(), DeviceTokenId::string(), Payload::binary(), CreationTime::datetime(), DeliveryTime::datetime()]).
-compile(export_all).

-belongs_to(app).
-belongs_to(device_token).

before_create() ->
    ModifiedRecord = set([{creation_time, calendar:universal_time()}]),
    {ok, ModifiedRecord}.