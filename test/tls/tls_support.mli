(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** Test certificates for weft-tls: a CA and a leaf for [localhost], with P-256
    keys. The keys come from the default generator of mirage-crypto-rng. *)

val pki :
     ?names:string list
  -> ?ips:Ipaddr.t list
  -> unit
  -> X509.Certificate.t * X509.Certificate.t * X509.Private_key.t
(** [pki ~names ~ips ()] is a new CA, a leaf certificate for the DNS names
    [names] (default [["localhost"]], not empty) and the addresses [ips]
    (default none) that the CA signed, and the key of the leaf. The certificates
    are valid from 2020 to 2040. *)

val localhost : [`host] Domain_name.t

val configs :
     ?peer_name:[`host] Domain_name.t
  -> time:(unit -> Ptime.t option)
  -> unit
  -> Tls.Config.server * Tls.Config.client
(** [configs ~time ()] makes a {!pki}, and gives a server configuration with the
    leaf and a client configuration that trusts the CA at the time [time ()].
    The peer name of the client is [peer_name] (default {!localhost}). *)
