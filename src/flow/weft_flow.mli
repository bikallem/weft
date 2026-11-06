(** Byte streams on weft (design section 2.18).

    A protocol reads its input from a {!Flow.t} and writes its output to it.
    {!Tcp} gives the flows of TCP and Unix stream sockets, and the listeners.
    Another layer, for example TLS, makes a flow from another flow with
    {!Flow.make}.

    The library uses only the core of weft. Thus the same code runs in each
    world of the OS and in the simulator. *)

module Flow = Flow
module Tcp = Tcp
