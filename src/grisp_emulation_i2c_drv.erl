-module(grisp_emulation_i2c_drv).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> undefined.

% FIXME: Placeholder API
command(undefined, Things) ->
    grisp_emulation_device:broadcast({i2c, Things}),
    <<0>>.
