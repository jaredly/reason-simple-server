# Reason Simple Server

This is a library for native reason (with bsb-native) that lets you stand up a simple server without too much trouble.

## Basic Server

```
open ReasonSimpleServer.Basic.Response;

let handler = (method, path, headers) => {
  switch (method, path) {
  | ("GET", "/") => {
    Ok("text/plain", "All clear boss!")
  }
  | ("POST", _) => Bad(401, "Can't do that")
  | ("Get", "/") => Ok("text/html", "<h1>Howdy</h1>")
  }
};

ReasonSimpleServer.Basic.listen(~port=3451, handler);
```

There's lots of features missing, but it's very small and light.

## Static Server

```
ReasonSimpleServer.Static.run(~port=3451, "./");
```