(* The in-memory file system of the simulator (design section 2.8). It copies
   the errors that the API documents: ENOENT, EEXIST, ENOTDIR, EISDIR, ENOTEMPTY
   and EBADF (open question 1). It has no effects. *)

open Weft.Private

type node = File of file | Dir of dir

and file =
  {mutable data: Bytes.t; mutable size: int; fperm: int; mutable fmtime: int64}

and dir = {entries: (string, node) Hashtbl.t; dperm: int; dmtime: int64}

(* An open file. *)
type handle = {node: node; mutable pos: int; rd: bool; wr: bool; append: bool}

type t = {root: dir}

let new_dir perm now =
  {entries= Hashtbl.create ~random:false 8; dperm= perm; dmtime= now}

let create () = {root= new_dir 0o755 0L}

let ( let* ) = Result.bind

(* The components of a path, with no empty names and no ".". A path is relative
   to the root. *)
let components path =
  List.filter (fun c -> c <> "" && c <> ".") (String.split_on_char '/' path)

(* The directory of the components, with ".." to the parent. *)
let walk t comps =
  let rec go stack = function
    | [] ->
        Ok stack
    | ".." :: rest ->
        go (match stack with [_] -> stack | _ :: up -> up | [] -> []) rest
    | name :: rest -> (
      match Hashtbl.find_opt (List.hd stack).entries name with
      | Some (Dir d) ->
          go (d :: stack) rest
      | Some (File _) ->
          Error Op.Not_dir
      | None ->
          Error Op.No_entry )
  in
  let* stack = go [t.root] comps in
  Ok (List.hd stack)

(* The place of a path: [In (d, name)] is the directory that holds the last
   component, and its name. A path with no last name, as the root or a path that
   ends with "..", gives [Top d], with the directory that the path names. *)
type place = Top of dir | In of dir * string

let parent t path =
  if path = "" then Error Op.No_entry
  else
    let rec split = function
      | [] ->
          None
      | [".."] ->
          None
      | [last] ->
          Some ([], last)
      | c :: rest ->
          Option.map (fun (dirs, last) -> (c :: dirs, last)) (split rest)
    in
    match split (components path) with
    | None ->
        let* d = walk t (components path) in
        Ok (Top d)
    | Some (dirs, last) ->
        let* d = walk t dirs in
        Ok (In (d, last))

(* A path that ends with "/" names a directory. *)
let dir_only path = path <> "" && path.[String.length path - 1] = '/'

(* The largest size of a file, as a limit of the memory of the simulator. *)
let max_size = 1 lsl 40

let lookup t path =
  let* p = parent t path in
  match p with
  | Top d ->
      Ok (Dir d)
  | In (d, name) -> (
    match Hashtbl.find_opt d.entries name with
    | Some (File _) when dir_only path ->
        Error Op.Not_dir
    | Some n ->
        Ok n
    | None ->
        Error Op.No_entry )

let open_ t path flags perm ~now =
  let has f = List.mem f flags in
  let wr = has Op.O_write in
  let rd = has Op.O_read || not wr in
  let* p = parent t path in
  let handle node = {node; pos= 0; rd; wr; append= has Op.O_append} in
  match p with
  | Top d ->
      if wr then Error Op.Is_dir else Ok (handle (Dir d))
  | In (d, name) -> (
    match Hashtbl.find_opt d.entries name with
    | Some _ when has Op.O_create && has Op.O_exclusive ->
        Error Op.Exists
    | Some (Dir _) when wr || has Op.O_create ->
        Error Op.Is_dir
    | Some (File _) when dir_only path ->
        Error Op.Not_dir
    | Some (File f as n) ->
        if has Op.O_truncate && wr then (
          f.size <- 0 ;
          f.fmtime <- now ) ;
        Ok (handle n)
    | Some (Dir _ as n) ->
        Ok (handle n)
    | None when dir_only path && has Op.O_create ->
        Error Op.Is_dir
    | None ->
        if has Op.O_create then (
          let n =
            File {data= Bytes.create 64; size= 0; fperm= perm; fmtime= now}
          in
          Hashtbl.replace d.entries name n ;
          Ok (handle n) )
        else Error Op.No_entry )

let file_of h = match h.node with File f -> Ok f | Dir _ -> Error Op.Is_dir

(* [at] is the file offset, or [-1] for the offset of the handle. *)
let pread h len at =
  if not h.rd then Error Op.Bad_fd
  else
    let* f = file_of h in
    let p = if at < 0 then h.pos else at in
    let n = Int.max 0 (Int.min len (f.size - p)) in
    if at < 0 then h.pos <- p + n ;
    (* A read at or past the end gives 0 bytes. *)
    Ok (if n = 0 then "" else Bytes.sub_string f.data p n)

let grow f size =
  if (size > Bytes.length f.data) [@mutate off "a copy more at the same size"]
  then (
    let b = Bytes.make (Int.max size (2 * Bytes.length f.data)) '\000' in
    Bytes.blit f.data 0 b 0 f.size ;
    f.data <- b )

let pwrite h data at ~now =
  if not h.wr then Error Op.Bad_fd
  else
    let* f = file_of h in
    let p = if h.append then f.size else if at < 0 then h.pos else at in
    let n = String.length data in
    if
      ( p + n > max_size )
      [@mutate
        off "a limit of the memory of the simulator: a test would allocate it"]
    then Error (Op.Other "EFBIG")
    else (
      grow f (p + n) ;
      (* A write past the end leaves zeros in the gap. *)
      if (p > f.size) [@mutate off "a fill of 0 bytes"] then
        Bytes.fill f.data f.size (p - f.size) '\000' ;
      Bytes.blit_string data 0 f.data p n ;
      f.size <- Int.max f.size (p + n) ;
      f.fmtime <- now ;
      if at < 0 then h.pos <- p + n ;
      Ok n )

(* Linux gives EINVAL for an fd that is not open for write. *)
let ftruncate h size ~now =
  if not h.wr then Error (Op.Other "EINVAL")
  else if
    ( size > max_size )
    [@mutate
      off "a limit of the memory of the simulator: a test would allocate it"]
  then Error (Op.Other "EFBIG")
  else
    let* f = file_of h in
    grow f size ;
    if (size > f.size) [@mutate off "a fill of 0 bytes"] then
      Bytes.fill f.data f.size (size - f.size) '\000' ;
    f.size <- size ;
    f.fmtime <- now ;
    Ok ()

let stat_of = function
  | File f ->
      {Op.kind= Regular; size= f.size; perm= f.fperm; mtime= f.fmtime}
  | Dir d ->
      {Op.kind= Directory; size= 4096; perm= d.dperm; mtime= d.dmtime}

let stat t path = Result.map stat_of (lookup t path)

let mkdir t path perm ~now =
  let* p = parent t path in
  match p with
  | Top _ ->
      Error Op.Exists
  | In (d, name) ->
      if Hashtbl.mem d.entries name then Error Op.Exists
      else (
        Hashtbl.replace d.entries name (Dir (new_dir perm now)) ;
        Ok () )

let readdir t path =
  let* n = lookup t path in
  match n with
  | Dir d ->
      Ok (Hashtbl.fold (fun name _ acc -> name :: acc) d.entries [])
  | File _ ->
      Error Op.Not_dir

let unlink t path =
  let* p = parent t path in
  match p with
  | Top _ ->
      Error Op.Is_dir
  | In (d, name) -> (
    match Hashtbl.find_opt d.entries name with
    | Some (File _) when dir_only path ->
        Error Op.Not_dir
    | Some (File _) ->
        Hashtbl.remove d.entries name ;
        Ok ()
    | Some (Dir _) ->
        Error Op.Is_dir
    | None ->
        Error Op.No_entry )

let rmdir t path =
  (* Linux refuses "." as the last component. *)
  let last =
    List.rev (List.filter (( <> ) "") (String.split_on_char '/' path))
  in
  if List.nth_opt last 0 = Some "." then Error (Op.Other "EINVAL")
  else
    let* p = parent t path in
    match p with
    | Top _ ->
        Error (Op.Other "EBUSY")
    | In (d, name) -> (
      match Hashtbl.find_opt d.entries name with
      | Some (Dir e) ->
          if Hashtbl.length e.entries > 0 then Error Op.Not_empty
          else (
            Hashtbl.remove d.entries name ;
            Ok () )
      | Some (File _) ->
          Error Op.Not_dir
      | None ->
          Error Op.No_entry )

(* [dir] is [x] or a directory under [x]. *)
let rec inside dir x =
  dir == x
  || Hashtbl.fold
       (fun _ n acc ->
         acc || match n with Dir d -> inside dir d | File _ -> false )
       x.entries false

let rename t src dst =
  let* ps = parent t src in
  let* pd = parent t dst in
  match (ps, pd) with
  | Top _, _ | _, Top _ ->
      Error (Op.Other "EBUSY")
  | In (sd, sname), In (dd, dname) -> (
    match Hashtbl.find_opt sd.entries sname with
    | None ->
        Error Op.No_entry
    | Some n -> (
        let move () =
          Hashtbl.remove sd.entries sname ;
          Hashtbl.replace dd.entries dname n ;
          Ok ()
        in
        match (n, Hashtbl.find_opt dd.entries dname) with
        | _, Some n' when n' == n ->
            Ok ()
        | Dir d, _ when inside dd d ->
            Error (Op.Other "EINVAL")
        | File _, Some (Dir _) ->
            Error Op.Is_dir
        | Dir _, Some (File _) ->
            Error Op.Not_dir
        | Dir _, Some (Dir e) when Hashtbl.length e.entries > 0 ->
            Error Op.Not_empty
        | _ ->
            move () ) )
