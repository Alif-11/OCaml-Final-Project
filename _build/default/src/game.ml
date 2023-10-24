(** LIFTED WordBag FROM A2: bag.ml, line 15 Sampleable bag of words such that
    sample returns elements with probability proportional to their multiplicity. *)
    module WordBag = struct
      type t = string list
    
      let to_list (b : t) : t = b
      let of_list (lst : t) : t = lst
      let join (b1 : t) (b2 : t) : t = List.append b1 b2
    
      let sample (b : t) : string option =
        match b with
        | [] -> None
        | _ -> Some (List.length b |> Random.int |> List.nth b)
    end

(** LIFTED <sanitize (s: string) : string list> FROM A2: ngrams.ml, line 69
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

let word_bag = WordBag.of_list data

(** generates a random sequence of words *)
let rec generate_sequence (words : WordBag.t) (n : int) : string list =
  if n > 0 then
    let w = WordBag.sample words in
    match w with
    | None -> []
    | Some word -> word :: generate_sequence words (n - 1)
  else []
