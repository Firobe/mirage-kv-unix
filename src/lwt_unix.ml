let wrap1 f a = Lwt.return (f a)
let wrap2 f a b = Lwt.return (f a b)
let wrap3 f a b c = Lwt.return (f a b c)
let wrap4 f a b c d = Lwt.return (f a b c d)

include Unix

let openfile = wrap3 openfile
let read = wrap4 read
let close = wrap1 close
let mkdir = wrap2 mkdir
let unlink = wrap1 unlink
let rmdir = wrap1 rmdir
let write = wrap4 write
let utimes = wrap3 utimes
let rename = wrap2 rename

module LargeFile = struct
  include LargeFile

  let lseek = wrap3 lseek
  let fstat = wrap1 fstat
  let stat = wrap1 stat
end

let files_of_directory path =
  let open Unix in
  let handle = opendir path in
  let s, push = Lwt_stream.create () in
  let rec next () =
    try
      let content = readdir handle in
      push (Some content);
      next ()
    with End_of_file -> push None
  in
  next ();
  s
