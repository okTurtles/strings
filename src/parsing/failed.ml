open! Core

type t = {
  filename: string;
  message: string;
}

let to_string { filename; message } = sprintf "Parsing error in %s:\n%s" filename message
