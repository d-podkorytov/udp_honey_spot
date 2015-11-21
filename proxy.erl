-module(proxy).
-compile(export_all).
-define(SERVERS,[{{77,88,8,3},53}]).
-define(TIMEOUT,2000).

%trivial proxy handler no cahing no logging no statistic

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_query_tu(Socket, 
                ClientIP, 
                Port,
                {ok,{dns_rec,Header,[{dns_query,Name,Class,Type}],_,_,_
                    }
                }
               )
 ->

 io:format("~p:~p query ~p ~n",[?MODULE,?LINE,{Name,Class,Type,{date(),time()}}]),
 io:format("~p:~p resolved ~p ~n",[?MODULE,?LINE,inet_res:resolve(Name,Type,Class)]),
 io:format("~p:~p nnslookup ~p ~n",[?MODULE,?LINE,inet_res:nnslookup(Name,Type,Class,?SERVERS,?TIMEOUT)]),

 {ok,R}=inet_res:resolve(Name,Type,Class),
 
 Rbin=inet_dns:encode(R),
 ID=tdns_ut:get_header_id(Header), % or from Bbin for more fast

 R_out=tdns_ut:inject_id(ID,Rbin),
  gen_udp:send(Socket,ClientIP,Port,R_out);

handle_query_tu(Socket,ClientIP,Port,Inp) -> 
  io:format("~p:~p unknown input ~p ~n",[?MODULE,?LINE,Inp]).

