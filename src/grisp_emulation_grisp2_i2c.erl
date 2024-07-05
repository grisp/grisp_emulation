-module(grisp_emulation_grisp2_i2c).

% API
-export([i2c_open_nif/1]).
-export([i2c_transfer_nif/2]).

%--- API -----------------------------------------------------------------------

i2c_open_nif([Bus, 0]) ->
    eeprom_spawn(som),
    eeprom_spawn(board),
    Bus.

i2c_transfer_nif(Bus, Messages) ->
    try
        transfer(Bus, Messages)
    catch
        not_emulated -> {error, not_emulated};
        device_missing -> {error, {ioctl_failed, "Device not found"}}
    end.

%--- Internal ------------------------------------------------------------------

binary_pad(Bin, Size) when is_binary(Bin), is_integer(Size), Size >= 0 ->
    BinSize = byte_size(Bin),
    if
        BinSize >= Size -> Bin;
        true ->
            PaddingSize = Size - BinSize,
            Padding = <<0:(8*PaddingSize)>>,
            <<Bin/binary, Padding/binary>>
    end.

eeprom_procname(Tag) ->
    list_to_atom("emulated_eeprom_" ++ atom_to_list(Tag)).

eeprom_spawn(Tag) ->
    ProcName = eeprom_procname(Tag),
    case erlang:whereis(ProcName) of
        Pid when is_pid(Pid) -> Pid;
        undefined ->
            erlang:spawn_link(fun() -> eeprom_proc_init(ProcName, Tag) end)
    end.

eeprom_proc_init(ProcName, Tag) ->
    Filename = io_lib:format("eeprom_~s.dat", [Tag]),
    {ok, File} = file:open(Filename, [read, write, raw, binary]),
    process_flag(trap_exit, true),
    erlang:register(ProcName, self()),
    eeprom_proc_loop(File).

eeprom_proc_loop(File) ->
    receive
        {read, From, Addr, Size} ->
            Data = case file:pread(File, Addr, Size) of
                eof -> <<0:(8*Size)>>;
                {ok, D} -> binary_pad(D, Size)
            end,
            From ! {read_result, Data},
            eeprom_proc_loop(File);
        {write, From, Addr, Data} ->
            ok = file:pwrite(File, Addr, Data),
            From ! {write_result, ok},
            eeprom_proc_loop(File);
        {'EXIT', _From, _Reason} ->
            file:close(File)
    end.

eeprom_read(Tag, Addr, Size) ->
    Procname = eeprom_procname(Tag),
    Procname ! {read, self(), Addr, Size},
    receive {read_result, Data} -> Data end.

eeprom_write(Tag, Addr, Data) ->
    Procname = eeprom_procname(Tag),
    Procname ! {write, self(), Addr, Data},
    receive {write_result, Result} -> Result end.

eeprom_bin_to_addr(<<>>) -> undefined;
eeprom_bin_to_addr(<<Addr:8>>) -> Addr;
eeprom_bin_to_addr(<<Addr:16/big-integer>>) -> Addr.

transfer(Bus, Messages) ->
    State = #{
        eeprom_som_addr => undefined,
        eeprom_board_addr => undefined
    },
    transfer(State, Bus, Messages, []).

transfer(_State, _Bus, [], Acc) ->
    lists:reverse(Acc);
transfer(State, Bus, [Message | Rest], Acc) ->
    {Result, State2} = transfer(State, Bus, Message),
    transfer(State2, Bus, Rest, [Result | Acc]).

%% SOM EEPROM I2C EMULATION
transfer(State, <<"/dev/i2c-0">>, {write, 16#52, 0, AddrBin}) ->
    {ok, State#{eeprom_som_addr := eeprom_bin_to_addr(AddrBin)}};
transfer(State = #{eeprom_som_addr := Addr},
         <<"/dev/i2c-0">>, {write, 16#52, 16#4000, Data})
  when Addr =/= undefined ->
    Result = eeprom_write(som, Addr, Data),
    {Result, State#{eeprom_som_addr := undefined}};
transfer(State = #{eeprom_som_addr := Addr},
         <<"/dev/i2c-0">>, {read, 16#52, 1, Size})
  when Addr =/= undefined ->
    Result = eeprom_read(som, Addr, Size),
    {Result, State#{eeprom_som_addr := undefined}};
%% BOARD EEPROM I2C EMULATION
transfer(State, <<"/dev/i2c-0">>, {write, 16#57, 0, AddrBin}) ->
    {ok, State#{eeprom_board_addr := eeprom_bin_to_addr(AddrBin)}};
transfer(State = #{eeprom_board_addr := Addr},
         <<"/dev/i2c-0">>, {write, 16#57, 16#4000, Data})
  when Addr =/= undefined ->
    Result = eeprom_write(board, Addr, Data),
    {Result, State#{eeprom_board_addr := undefined}};
transfer(State = #{eeprom_board_addr := Addr},
         <<"/dev/i2c-0">>, {read, 16#57, 1, Size})
  when Addr =/= undefined ->
    Result = eeprom_read(board, Addr, Size),
    {Result, State#{eeprom_board_addr := undefined}};
%% BACKWARD COMPATIBLE DS2482 EMULATION
transfer(State, <<"/dev/i2c-0">>, {write, 16#18, 0, <<>>}) ->
    {ok, State};
transfer(_State, <<"/dev/i2c-0">>, _) ->
    throw(device_missing);
transfer(_State, _, _) ->
    throw(not_emulated).
