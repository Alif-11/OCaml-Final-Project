(** 
LIFTED <sanitize (s: string) : string list> FROM A2: ngrams.ml, line 69
Sanitize a string by removing non-alphanumeric symbols. Return the list of
    words obtained by splitting on spaces. *)
let sanitize (s : string) : string list =
  s
  |> Str.global_replace (Str.regexp "[ \t\r\n]") " "
  |> Str.global_replace (Str.regexp "[^a-zA-Z0-9' ]") ""
  |> String.lowercase_ascii |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  
let data_loader (filename : string) : string list =
  In_channel.open_text filename |> In_channel.input_all |> sanitize
let data = data_loader "data/1-1000.txt"

