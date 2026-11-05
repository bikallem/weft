(* The disk use of a directory tree. Each directory is a scope, and each entry
   is a fiber of that scope. The code uses only the API of weft, so it runs in
   each world. *)

open Weft

type stats =
  { files: int
  ; dirs: int
  ; bytes: int
  ; errors: int  (** The entries that a stat or a readdir did not read. *)
  ; peak: int  (** The most operations that ran at the same time. *) }

(* Walk the tree at [root]. A [File] or [Dir] operation runs on the thread pool
   of the world, and many fibers can start operations at the same time. The
   semaphore lets [limit] operations run at the same time, thus a large tree
   does not fill the pool.

   [File.stat] follows a symbolic link. The OS stops a loop of links with an
   error (ELOOP), and [du] counts it in [errors]. *)
(* A path in the directory [dir]. [Filename.concat] puts a "\\" on Windows, but
   the file system of the simulator knows only "/". Windows also accepts "/",
   thus [concat] uses "/" on each system. *)
let concat dir name =
  if String.ends_with ~suffix:"/" dir then dir ^ name else dir ^ "/" ^ name

let du ?(limit = 16) root =
  let ops = Fiber.Semaphore.create limit in
  let files = Atomic.make 0
  and dirs = Atomic.make 0
  and bytes = Atomic.make 0
  and errors = Atomic.make 0
  and running = Atomic.make 0
  and peak = Atomic.make 0 in
  let io f =
    Fiber.Semaphore.use ops (fun () ->
        let n = Atomic.fetch_and_add running 1 + 1 in
        let rec raise_peak () =
          let p = Atomic.get peak in
          if n > p && not (Atomic.compare_and_set peak p n) then raise_peak ()
        in
        raise_peak () ;
        Fun.protect ~finally:(fun () -> Atomic.decr running) f )
  in
  let rec walk path =
    match io (fun () -> File.stat path) with
    | exception File.Error _ ->
        Atomic.incr errors
    | {kind= Directory; _} -> (
        Atomic.incr dirs ;
        match io (fun () -> Dir.readdir path) with
        | exception Dir.Error _ ->
            Atomic.incr errors
        | names ->
            (* The scope ends when the walk of each entry ends. *)
            Fiber.scope (fun s ->
                List.iter
                  (fun name -> Fiber.fork s (fun () -> walk (concat path name)))
                  names ) )
    | {kind= Regular; size; _} ->
        Atomic.incr files ;
        ignore (Atomic.fetch_and_add bytes size)
    | {kind= Symlink | Special; _} ->
        ()
  in
  walk root ;
  { files= Atomic.get files
  ; dirs= Atomic.get dirs
  ; bytes= Atomic.get bytes
  ; errors= Atomic.get errors
  ; peak= Atomic.get peak }
