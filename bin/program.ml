open Opium
open Tyxml.Html

let fizzbuzz count =
  let f x = match x with
  | _ when x mod 15 == 0 -> "FizzBuzz"
  | _ when x mod 5  == 0 -> "Buzz"
  | _ when x mod 3  == 0 -> "Fizz"
  | _                    -> string_of_int x
  in
  List.map f (List.init count succ)

let emit_page p =
  html
    (head (title (txt "test")) [])
    (body [p])

let fizzbuzz_web req =
  let count =
    int_of_string (Router.param req "count")
  in
  let r =
    fizzbuzz count |> List.map txt
  in
  let div =
    div (r |> List.map (fun x -> span [x; br ()]))
  in
  emit_page div
  |> Response.of_html
  |> Lwt.return

let sourcecode = a_href "https://github.com/ry00001/web_fizzbuzz"

let hello_world _req =
  let r =
    div [
      h1 [txt "Hello, world!"];
      p [
        txt "This is a test web application written in ";
        a ~a:[a_href "https://ocaml.org"] [txt "OCaml"];
        txt "."
      ];
      p [
        txt "Don't expect much here at all, ";
        txt "but if you really want to look, ";
        txt "the source code for this is available ";
        a ~a:[sourcecode] [txt "here"];
        txt ".";
      ]
    ]
  in
  emit_page r
  |> Response.of_html
  |> Lwt.return

let main =
  App.empty
  |> App.port 8086
  |> App.get "/" hello_world
  |> App.get "/fizzbuzz/:count" fizzbuzz_web
  |> App.run_command