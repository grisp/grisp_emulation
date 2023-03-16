-module(grisp_emulation_grisp2_spi).

% API
-export([open_nif/0]).
-export([ioctl_nif/4]).

%--- Macros --------------------------------------------------------------------

-define(CPOL_HIGH, 1).
-define(CPHA_TRAILING, 2).
-define(CS_DISABLE, 4).

%--- API -----------------------------------------------------------------------

open_nif() ->
    maps:from_list([{PinIndex, open(PinIndex)} || PinIndex <- lists:seq(0, 3)]).

ioctl_nif(State, CS, Mode, Msg) ->
    chip_select(State, CS, fun() ->
        grisp_emulation_device:message(slot(CS), {spi, CS, mode(Mode), Msg})
    end).

%--- Internal ------------------------------------------------------------------

% FIXME: We probably don't handle custom pins like {gpio, Pin} here!
slot(0) -> spi1;
slot(1) -> spi2;
slot(2) -> spi2;
slot(3) -> spi2.

mode(0) -> #{clock => {low, leading}};
mode(?CPHA_TRAILING) -> #{clock => {low, trailing}};
mode(?CPOL_HIGH) -> #{clock => {high, leading}};
mode(?CPOL_HIGH bor ?CPHA_TRAILING) -> #{clock => {high, trailing}}.

chip_select(State, CS, Fun) ->
    Pin = maps:get(CS, State),
    set(Pin, 0),
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            set(Pin, 1),
            erlang:raise(Class, Reason, Stacktrace)
    end.

open(PinIndex) ->
    grisp_emulation_grisp2_gpio:gpio_open_nif(pin(PinIndex), {output, 1}).

set(Pin, Value) ->
    grisp_emulation_grisp2_gpio:gpio_set_nif(Pin, Value).

pin(0) ->
    #{path => <<"/pmod-spi\0">>, property => <<"grisp,gpios\0">>, index => 8};
pin(1) ->
    #{path => <<"/pmod-spi\0">>, property => <<"grisp,gpios\0">>, index => 0};
pin(2) ->
    #{path => <<"/pmod-spi\0">>, property => <<"grisp,gpios\0">>, index => 6};
pin(3) ->
    #{path => <<"/pmod-spi\0">>, property => <<"grisp,gpios\0">>, index => 7}.
