% DNS service based on OTP and FP
% OTP UDP service 

-module(honey_udp).
-behaviour(supervisor).

-include("tdns_fp.hrl").
-compile(export_all). 

-define(T(),io:format("~p:~p ~n",[?MODULE,?LINE])).
-define(TIMEOUT,1000).
-define(SERVERS, [{{77,88,8,3},53}]).
 
% Behaviour Callbacks

behaviour_info(callbacks) -> [{handle_call, 4}, {handle_info, 2}];
behaviour_info(_Other) -> undefined.
 
% Supervisor
%% External APIs

start_link(Module) -> 
 ?T(),
 start_link(Module, #udp_server_option{}).
 
start_link(RegistName, Module) when is_atom(Module) ->
 ?T(),
  start_link(RegistName, Module, #udp_server_option{});

start_link(Module, Option) ->
 ?T(),
  start_link({local, ?MODULE}, Module, Option).
 
start_link({Dest, Name}=RegistName, Module, Option) ->
  ?T(),
  supervisor:start_link(
    {Dest, supervisor_name(Name)},
    ?MODULE,
    [RegistName, Module, Option]
  ).
 
stop() -> 
 ?T(),
 stop(?MODULE).
 
stop(Name) ->
   ?T(),
  case whereis(supervisor_name(Name)) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _ -> not_started
  end.
 
supervisor_name(Name) when is_atom(Name)-> 
    ?T(),
    list_to_atom(atom_to_list(Name) ++ "_sup").
 
%% Callbacks
init([{_Dest, Name}=RegistName, Module, Option]) ->
  ?T(),
  #udp_server_option{
    max_restarts = MaxRestarts,
    time = Time,
    shutdown = Shutdown
  } = Option,
  {ok, {{one_for_one, MaxRestarts, Time}, [
    {
      Name,
      {?MODULE, receiver_start_link, [RegistName, Module, Option]},
      permanent,
      Shutdown,
      worker,
      []
    }
  ]}}.
 
% ProcLib - udp_server_receiver
%% External APIs

receiver_start_link({Dest, Name}, Module, Option) ->
  ?T(),
  {ok, Pid}
    = proc_lib:start_link(?MODULE, receiver_init, [self(), Module, Option]),
  case Dest of
    local -> register(Name, Pid);
    _Global -> global:register_name(Name, Pid)
  end,
  {ok, Pid}.
 
%% Callbacks
receiver_init(Parent, Module, Option) ->
  ?T(),
  case gen_udp:open(
    Option#udp_server_option.port,
    Option#udp_server_option.option
  ) of
    {ok, Socket} ->
      proc_lib:init_ack(Parent, {ok, self()}),
      recv(
        proplists:get_value(active, Option#udp_server_option.option),
        Socket, Module, Option
      );
    {error, Reason} -> exit({error, Reason})
  end.
 
recv(false, Socket, Module, Option) ->
  ?T(),
  case gen_udp:recv(
    Socket,
    Option#udp_server_option.recv_length,
    Option#udp_server_option.recv_timeout
  ) of
    {ok, {Address, Port, Packet}} ->
      Module:handle_call(Socket, Address, Port, Packet),
      recv(false, Socket, Module, Option);
    {error, Reason} -> exit({error, Reason})
  end;
 
recv(_Active, Socket, Module, Option) ->
  
  io:format("~p:~p Opt=~p ~n",[?MODULE,?LINE,Option]),
  receive
    {udp, Socket, Address, Port, Packet} ->
      Module:handle_call(Socket, Address, Port, Packet,Option), %+Option
      recv(true, Socket, Module, Option);
    OtherMessage ->
      Module:handle_info(Socket, OtherMessage),
      recv(true, Socket, Module, Option)
  after Option#udp_server_option.recv_timeout ->
    exit({error, udp_timeout})
  end.

%handle_call(Socket, ClientIP, Port, Packet)->
% ?T(),
% io:format("~p:~p ~p ~n",[?MODULE,?LINE,{now(),Socket, ClientIP, Port, inet_dns:decode(Packet)}]),
% F4=fun(A,B,C,D)-> handle_query_tu(A,B,C,D) end,
% F4(Socket, ClientIP, Port,inet_dns:decode(Packet)).

handle_call(Socket, ClientIP, Port, Packet,Opt)->
 ?T(),
 io:format("~p:~p ~p ~n",[?MODULE,?LINE,{{date(),time()},Socket, ClientIP, Port, inet_dns:decode(Packet)}]),
 F4=Opt#udp_server_option.handler4,
 F4(Socket, ClientIP, Port,inet_dns:decode(Packet)).

start()->
 ?MODULE:start_link(?MODULE,#udp_server_option 
   {handler4=fun(A,B,C,D)-> proxy:handle_query_tu(A,B,C,D) end
   }
  ).

start(F4) when is_function(F4,4)->
 ?MODULE:start_link(?MODULE,#udp_server_option {handler4=F4} ).

start(F4,Port) when is_function(F4,4) and is_integer(Port)-> 
 ?MODULE:start_link(?MODULE,#udp_server_option {handler4=F4,port=Port} );

start(_F4,[]) ->ok;
start(F4,[Port|Tail]) when is_function(F4,4) and is_integer(Port)-> 
 start(F4,Port),
 start(F4,Tail).

% start service at port range From To
start(F4,From,To)-> start(F4,lists:seq(From,To)).
