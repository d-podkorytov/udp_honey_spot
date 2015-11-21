Erlang echo UDP server based on OTP.

It can be used as honey spot but need some addition for like logging and alertrs subsystems.

Just an example for my students and as a point to upstair for next developing.

For running it on all UDP ports just:

1) compile it: 
   $erlc *.erl
2) run ERLANG VM 
   $sudo erl
3) type honey_udp:start(fun(A,B,C,D)-> proxy:handle_query_tu(A,B,C,D) end,1,65535).

If running it on single port default port is 53.
