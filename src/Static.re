
let mime_for_name = ext => switch (String.lowercase(ext)) {
| "txt" => "text/plain"
| "html" => "text/html"
| "json" => "text/json"
| "js" => "application/javascript"
| "jpg" | "jpeg" => "image/jpeg"
| "png" => "image/png"
| "pdf" => "image/pdf"
| "ico" => "image/ico"
| "gif" => "image/gif"
| _ => "application/binary"
};

let ext = path => {
  let parts = Str.split(Str.regexp("\\."), path);
  List.nth(parts, List.length(parts) - 1)
};

let unwrap = (message, opt) => switch opt { | Some(x) => x | None => failwith(message)};

open Basic.Response;

let sendFile = (path, full_path) => switch (ReasonCliTools.Files.readFile(full_path)) {
| Some(text) => Ok(mime_for_name(ext(full_path)), text)
| None => Bad(404, "File not found: " ++ path)
};

let directoryListing = path => {
  let head = "
  <!doctype html><meta charset=utf8>
  <meta name='viewport' content='initial-scale=1, maximum-scale=1'>
  <style>
  ul { padding: 0; margin: 0; }
  li { list-style: none; }
  a {
    display: block;
    padding: 10px 20px;
    text-decoration: none;
    font-family: sans-serif;
  }
  a:hover { background-color: #eee; }
  </style>
  ";
  let list = ReasonCliTools.Files.readDirectory(path)
  |> List.map(name =>  "<a href='" ++ name ++ (ReasonCliTools.Files.isDirectory(Filename.concat(path, name)) ? "/" : "") ++ "'>" ++ name ++ "</a>")
  |> String.concat("</li><li>");
  head
  ++ "<ul><li><a href='..'>..</a></li><li>"
  ++ list
  ++ "</li></ul>"
};

let serveStatic = (~extraHandler=?, full_path, path, headers) => {
  switch (Unix.stat(full_path)) {
  | exception Unix.Unix_error(Unix.ENOENT, _, _) => Bad(404, "File not found: " ++ path)
  | stat =>
  switch (stat.Unix.st_kind) {
  | Unix.S_REG => sendFile(path, full_path)
  | Unix.S_DIR => {
    let index = Filename.concat(full_path, "index.html");
    if (ReasonCliTools.Files.isFile(index)) {
      sendFile(path, index)
    } else {
      Ok("text/html", directoryListing(full_path))
    }
  }
  | _ => Bad(404, "Unexpected file type: " ++ path)
  };
  }
};

let handler = (~extraHandler, base, method, path, headers) => {
  let res = switch (extraHandler) {
  | None => None
  | Some(handler) => handler(method, path, headers)
  };
  switch res {
  | Some(thing) => thing
  | None =>
  switch (method) {
  | "GET" => {
    let full_path = Filename.concat(base, "." ++ path);
    serveStatic(~extraHandler=?, full_path, path, headers)
  }
  | _ => Bad(401, "Method not allowed: " ++ method)
  }
  }
};

let run = (~extraHandler=?, ~poll=?, ~port, path) => Basic.listen(~poll=?poll, ~port, handler(~extraHandler, path));