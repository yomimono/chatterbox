open Lwt
open V1_LWT

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow

  let rec echo c flow =
    S.TCPV4.read flow >>= fun result -> (
      match result with  
        | `Eof -> report_and_close c flow "Echo connection closure initiated."
        | `Error e -> 
          let message = 
          match e with 
            | `Timeout -> "Echo connection timed out; closing.\n"
            | `Refused -> "Echo connection refused; closing.\n"
            | `Unknown s -> (Printf.sprintf "Echo connection error: %s\n" s)
             in
          report_and_close c flow message
        | `Ok buf ->
            S.TCPV4.write flow buf >>= fun () -> echo c flow
        ) 

  let start c s =
    (* RFC 862 - read payloads and repeat them back *)
    S.listen_tcpv4 s ~port:7 (echo c); 

    S.listen s
end

