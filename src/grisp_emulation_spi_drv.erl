-module(grisp_emulation_spi_drv).

% API
-export([open/0]).
-export([command/4]).

%--- API -----------------------------------------------------------------------

open() ->
    {ok, _} = grisp_emulation_device:start_link(),
    undefined.

command(_State, Slot, Mode, Command) ->
    grisp_emulation_device:message(Slot, {spi, Mode, Command}).
