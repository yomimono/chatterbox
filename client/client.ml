open Lwt
open V1_LWT
open OS

module Main (C: V1_LWT.CONSOLE) (CLIENT_STACK: V1_LWT.STACKV4) = struct
    let local_webserver="192.168.2.35"
    let port = 6667

  let start c client_stack =
    let construct_request () =
      let buf = Io_page.(to_cstruct (get 1)) in
      let output = (Printf.sprintf "land of adventure !!") in
      Cstruct.blit_from_string output 0 buf 0 (String.length output);
      Cstruct.set_len buf (String.length output)
    in
    
    let rec make_connection c s =
      let my_tcpv4 = (CLIENT_STACK.tcpv4 s) in
      let webserver = local_webserver in
        CLIENT_STACK.TCPV4.create_connection my_tcpv4 ((Ipaddr.V4.of_string_exn webserver),
        port) >>=
        fun conn -> (
      match conn with 
      | `Ok (outbound : CLIENT_STACK.TCPV4.flow) -> 
          let request = construct_request () in
          CLIENT_STACK.TCPV4.write outbound request >>
          CLIENT_STACK.TCPV4.read outbound >>= fun read_data -> (
            match read_data with
            | `Ok buffer -> C.log_s c (Cstruct.to_string buffer)
            | _ -> return ()
          ) >>= fun () -> 
          CLIENT_STACK.TCPV4.close outbound 
      | p -> 
          (* continue retrying until successful or heat death of universe *)
          C.log c (Printf.sprintf "Couldn't initiate connection to %s:%d ;
          retrying\n" webserver port);
          make_connection c s
    ) 
      in

    make_connection c client_stack (*what does it actually mean that the socket
    config is ignored? *)
end
