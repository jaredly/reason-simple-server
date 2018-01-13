
let recv = (client, maxlen) => {
  let bytes = Bytes.create(maxlen);
  let len = Unix.recv(client, bytes, 0, maxlen, []);
  Bytes.sub_string(bytes, 0, len)
};

let parse_top = top => {
  let parts = Str.split(Str.regexp("[ \t]+"), top);
  switch (parts) {
  | [method, path, ...others] => Some((method, path))
  | _ => None
  }
};

module StringMap = Map.Make(String);

let parse_headers = headers => {
  List.fold_left(
    (map, line) => {
      let parts = Str.split(Str.regexp(":"), line);
      switch parts {
      | [] | [_] => map
      | [name, ...rest] => StringMap.add(name, String.concat(":", rest), map)
      }
    },
    StringMap.empty,
    headers
  )
};

let parse_request = text => {
  let items = Str.split(Str.regexp("\r?\n"), text);
  switch items {
  | [] => failwith("Invalid request")
  | [top, ...headers] =>
  switch (parse_top(top)) {
  | None => failwith("Invalid top: " ++ top)
  | Some((method, path)) =>
  let header_map = parse_headers(headers);
  (method, path, header_map)
  }
  }
};

module Response = {
  type response =
    | Ok(string, string) /* mime, text */
    | Custom(Unix.file_descr => unit)
    | Bad(int, string); /* code, text */
};
open Response;

let formatResponse = (top, body) => {
  top ++ "\nServer: Ocaml-Cross-Mobile\nContent-length: " ++ string_of_int(String.length(body)) ++ "\n\n" ++ body
};

let okTop = mime => "HTTP/1.1 200 Ok\nContent-type: " ++ mime;
let badTop = code => "HTTP/1.1 " ++ string_of_int(code) ++ " Error\nContent-type: text/plain";

let canRead = desc => {
  let (r, w, e) = Unix.select([desc], [], [], 0.1);
  r != []
};

let sendToSocket = (client, text) => {
  let total = String.length(text);
  let left = ref(String.length(text));
  while (left^ > 0) {
    left := left^ - Unix.send(client, text, total - left^, left^, []);
  };
};

let listen = (~poll=?, ~port, handler) => {
  let sock = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(sock, Unix.SO_REUSEADDR, true);
  Unix.bind(sock, Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen(sock, 1000);

  let pollSock = () => {
    if (canRead(sock)) {
      let (client, source) = Unix.accept(sock);
      let request = recv(client,  1024);
      let response = try {
        let (method, path, header_map) = parse_request(request);

        handler(method, path, header_map);
      } {
        | _ => Bad(500, "Server error")
      };

      switch response {
      | Custom(cb) => {cb(client); None}
      | Ok(mime, body) => Some((okTop(mime), body))
      | Bad(code, body) => Some((badTop(code), body))
      } |> fun
      | None => ()
      | Some((top, body)) => {
        let fullText = formatResponse(top, body);
        sendToSocket(client, fullText);
        Unix.shutdown(client, Unix.SHUTDOWN_ALL);
      }
    }
  };

  while (true) {
    pollSock();

    switch(poll) {
    | Some(fn) => fn();
    | None => ()
    };
  }
};

