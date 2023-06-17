open! Core

type t = {
  path: string;
  filename: string;
  strings: string Queue.t;
  possible_scripts: string Queue.t;
  analyzed_possible_scripts: int;
  file_errors: string Queue.t;
}
[@@deriving sexp]

let create ~path =
  {
    path;
    filename = Filename.basename path;
    strings = Queue.create ();
    possible_scripts = Queue.create ();
    analyzed_possible_scripts = 0;
    file_errors = Queue.create ();
  }

let analyzed_possible_scripts collector =
  {
    collector with
    possible_scripts = Queue.create ~capacity:1 ();
    analyzed_possible_scripts = Queue.length collector.possible_scripts;
  }

let render_errors { file_errors; path; _ } =
  match Queue.length file_errors with
  | 0 -> None
  | 1 ->
    (* TODO: review/optimize *)
    let buf = Buffer.create 256 in
    bprintf buf "\n❌ 1 error in %s: %s" path (Queue.get file_errors 0);
    Some (Buffer.contents buf)
  | len ->
    let buf = Buffer.create 256 in
    bprintf buf "\n❌ %d errors in %s:\n" len path;
    Queue.iter file_errors ~f:(bprintf buf "- %s\n");
    Some (Buffer.contents buf)

let blit_transfer ~src ~dst =
  Queue.blit_transfer ~src:src.strings ~dst:dst.strings ();
  Queue.blit_transfer ~src:src.possible_scripts ~dst:dst.possible_scripts ();
  Queue.blit_transfer ~src:src.file_errors ~dst:dst.file_errors ()
