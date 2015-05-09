# TNA-7DTD
7 Days To Die TNA Dedicated Server Manager

# About This Implementation
Written for Common Lisp and heavily-dependent on the CL libraries - :bordeaux-
threads, :cl-ppcre, :telnetlib -- as well as on Alloc's open-source "Server
Fixes" - this expandable Server Manager base for any instance of the 7 Days To Die Dedicated Server concerns itself with practical issues first and lofty
bells and whistles only afterward. The :TNA-7DTD package features:

A listener that connects to a server instance's telnet-protocol-based back-end in order to parse, interpret and act on commands issued by users via the in-game chat. With this comes an elegant and organized set of essential commands as well
as the potentiality for easy addition of as many additional player-issuable mod-
con functionalities as is desired by the system operator.

The obvious integration of in-game chat with the IRC protocol, so that offline
community members do not miss out on the game &, perhaps more importantly, so
that multiple games/worlds/server instances can communicate with one-another,
creating the possibility for community growth on a larger scale.

System Resource Watch Automation. 7DTD is still in Alpha, and is developing such
that the dedicated servers are still programmed as a relative afterthought, i.e.
in need of much work e.g. to eliminate brutal memory leaking and other such non-
sense that results in process crash/hang, failure to save player/character and
world/environmental data. A modest threaded module of this package can warn the
game's occupants of impending crashes then quickly and safely shutdown and re-
start the server process without the System Operator's presence required, and
only when tests indicate a restart to be necessary, as in opposition to at an
interval i.e. on a clock.