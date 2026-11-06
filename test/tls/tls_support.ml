(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let ptime s = Option.get (Ptime.of_float_s s)

let dn cn =
  [ X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)) ) ]

let pki ?(names = ["localhost"]) ?(ips = []) () =
  let valid_from = ptime 1_577_836_800.
  and valid_until = ptime 2_208_988_800. in
  let ca_key = X509.Private_key.generate `P256 in
  let ca_dn = dn "weft test CA" in
  let ca =
    Result.get_ok
      (X509.Signing_request.sign
         (ok (X509.Signing_request.create ca_dn ca_key))
         ~valid_from ~valid_until
         ~extensions:
           X509.Extension.(
             empty
             |> add Basic_constraints (true, (true, None))
             |> add Key_usage (true, [`Key_cert_sign]) )
         ca_key ca_dn )
  in
  let key = X509.Private_key.generate `P256 in
  let leaf =
    Result.get_ok
      (X509.Signing_request.sign
         (ok (X509.Signing_request.create (dn (List.hd names)) key))
         ~valid_from ~valid_until
         ~extensions:
           X509.Extension.(
             empty
             |> add Subject_alt_name
                  ( false
                  , X509.General_name.(
                      let dns = singleton DNS names in
                      if ips = [] then dns
                      else
                        add IP (List.map (fun a -> Ipaddr.to_octets a) ips) dns )
                  )
             |> add Ext_key_usage (false, [`Server_auth])
             |> add Basic_constraints (true, (false, None)) )
         ca_key ca_dn )
  in
  (ca, leaf, key)

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

let configs ?(peer_name = localhost) ~time () =
  let ca, leaf, key = pki () in
  let server =
    ok (Tls.Config.server ~certificates:(`Single ([leaf], key)) ())
  in
  let authenticator = X509.Authenticator.chain_of_trust ~time [ca] in
  let client = ok (Tls.Config.client ~authenticator ~peer_name ()) in
  (server, client)
