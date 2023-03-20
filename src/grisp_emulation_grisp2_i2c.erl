-module(grisp_emulation_grisp2_i2c).

% API
-export([i2c_open_nif/1]).
-export([i2c_transfer_nif/2]).

%--- API -----------------------------------------------------------------------

i2c_open_nif([Bus, 0]) -> Bus.

i2c_transfer_nif(Bus, Messages) ->
    try
        [transfer(Bus, M) || M <- Messages]
    catch
        not_emulated -> {error, not_emulated};
        device_missing -> {error, {ioctl_failed, "Device not found"}}
    end.

%--- Internal ------------------------------------------------------------------

transfer(Bus, {write, Target, 0, <<>>}) ->
    case maps:is_key(Target, devices(Bus)) of
        true ->
            ok;
        _Else ->
            throw(device_missing)
    end;
transfer(_Bus, _Message) ->
    throw(not_emulated).

devices(<<"/dev/i2c-0">>) ->
    #{
        16#18 => ds2482,
        16#52 => eeprom_som,
        16#57 => eeprom_board
    }.
