module Irc = struct

  type prefix = 
    | Prefix of string
    | None

  type command = 
    | Ack_number
    | Ison of string
    | User of string
    | Nick of string
    | Pass of string
    | Privmsg of string (* TODO: this might be more helpfully modeled some
    other way *)

  type t = { 
    prefix: prefix;
    command: command;
    parameters: [Parameter of string];
  }

end
