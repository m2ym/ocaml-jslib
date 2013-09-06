open Jslib

module Html = Dom_html

let opt_get opt =
  Js.Opt.get opt (fun () -> failwith "empty")

let () =
  let d = Html.document in
  Html.window##onload <- Html.handler (fun _ ->
      let input = opt_get (Html.CoerceTo.textarea (opt_get (d##getElementById (Js.string "input")))) in
      let output = opt_get (d##getElementById (Js.string "output")) in
      let submit = opt_get (d##getElementById (Js.string "submit")) in
      submit##onclick <- Html.handler (fun _ ->
          let source = Js.to_string input##value in
          begin
            try
              let program = Parse5.parse_from_string source in
              let buf = Buffer.create 32 in
              let fmt = Format.formatter_of_buffer buf in
              Pretty.pp_print_program fmt program;
              Format.pp_print_flush fmt ();
              output##innerHTML <- Js.string (Buffer.contents buf)
            with _ ->
              Html.window##alert (Js.string "Parse error!")
          end;
          Js._true);
    Js._true)
