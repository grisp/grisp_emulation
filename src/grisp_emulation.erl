-module(grisp_emulation).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).
% API
-export([call/4]).

%--- Callbacks -----------------------------------------------------------------

start(_StartType, _StartArgs) -> grisp_emulation_sup:start_link().

stop(_State) -> ok.

%--- API -----------------------------------------------------------------------

call(Platform, Module, on_load, []) ->
    Impl = implementation(Platform, Module),
    try
        Impl:on_load()
    catch
        error:undef:ST ->
            case ST of
                [{Impl, on_load, [], _}|_] -> ok;
                _Else -> erlang:raise(error, undef, ST)
            end
    end;
call(Platform, grisp_hw, hw_platform_nif, []) ->
    Platform;
call(Platform, port, open, [Module, Owner, {spawn_driver, Name}, Settings]) ->
    Emu = list_to_atom("grisp_emulation_" ++ atom_to_list(Module)),
    grisp_emulation_device:call(Emu, open, [Platform, Owner, Name, Settings]);
call(Platform, port, command, [Module, Owner, Port, Command]) ->
    Emu = list_to_atom("grisp_emulation_" ++ atom_to_list(Module)),
    grisp_emulation_device:call(Emu, command, [Platform, Owner, Port, Command]);
call(Platform, Module, Function, Args) ->
    apply(implementation(Platform, Module), Function, Args).

%--- Internal ------------------------------------------------------------------

implementation(Platform, Module) ->
    <<"grisp_", Driver/binary>> = atom_to_binary(Module),
    binary_to_atom(<<
        "grisp_emulation_",
        (atom_to_binary(Platform))/binary,
        "_",
        Driver/binary
    >>).
