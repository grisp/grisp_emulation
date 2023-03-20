-module(grisp_emulation_grisp2_gpio).

% API
-export([on_load/0]).
-export([gpio_open_nif/2]).
-export([gpio_get_nif/1]).
-export([gpio_set_nif/2]).

-record(pin, {
    key,
    mode,
    value
}).

%--- API -----------------------------------------------------------------------

on_load() ->
    spawn(fun() ->
        ets:new(?MODULE, [named_table, public, {keypos, 2}]),
        register(?MODULE, self()),
        receive stop -> ok end
    end),
    ok.

gpio_open_nif(#{path := Path0, index := Index}, Mode) ->
    Path = denull(Path0),
    Pin = {Path, Index},
    ets:insert_new(?MODULE, #pin{
        key = Pin,
        mode = mode(Mode),
        value = value(Mode)
    }),
    Pin.

gpio_get_nif(Pin) -> ets:lookup_element(?MODULE, Pin, 3).

gpio_set_nif(Pin, Value) -> ets:update_element(?MODULE, Pin, {4, Value}).

%--- Internal ------------------------------------------------------------------

denull(Bin) ->
    Size = byte_size(Bin) - 1,
    case Bin of
        <<Actual:Size/binary, 0>> -> Actual;
        Else -> Else
    end.

mode({output, _Value}) -> output;
mode(Mode) -> Mode.

value(input) -> 0;
value({output, Value}) -> Value.
