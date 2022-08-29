type 'a cell = {
  mutable prev : 'a cell option;
  mutable next : 'a cell option;
  value : 'a;
}

type 'a t = {
  mutable first : 'a cell option;
  mutable cnt : 'a cell option;
  mutable last : 'a cell option;
}

exception Empty

let empty q =
  q.first <- None;
  q.last <- None;
  q.cnt <- None

let is_empty { first; _ } = first = None
let create () = { first = None; last = None; cnt = None }

let add is_first e pl =
  let dp = { prev = None; value = e; next = None } in
  match if is_first then pl.first else pl.last with
  | None ->
      pl.first <- Some dp;
      pl.last <- Some dp;
      pl.cnt <- Some dp
  | Some e ->
      if is_first then (
        e.prev <- Some dp;
        dp.next <- Some e;
        pl.first <- Some dp)
      else (
        e.next <- Some dp;
        dp.prev <- Some e;
        pl.last <- Some dp)

let prepend e pl = add true e pl
let append e pl = add false e pl

let delete is_last q =
  if q.cnt = None then raise Empty
  else
    let elt = Option.get (if is_last then q.last else q.first) in
    if Option.get q.last == Option.get q.first then (
      let res = elt.value in
      empty q;
      res)
    else
      let res = elt.value in
      if is_last then (
        (Option.get elt.prev).next <- None;
        q.last <- elt.prev)
      else (
        (Option.get elt.next).prev <- None;
        q.first <- elt.next);
      res

let delete_last (q : 'a t) : 'a = delete true q
let delete_first (q : 'a t) : 'a = delete false q

let mem (e : 'a) (q : 'a t) : bool =
  if is_empty q then false
  else
    let rec aux (current : 'a cell) =
      if current.value = e then true
      else
        let next = current.next in
        if next = None then false else aux (Option.get next)
    in
    aux (Option.get q.first)

let exists f (q : 'a t) : bool =
  if is_empty q then false
  else
    let rec aux (current : 'a cell) =
      if f current.value then true
      else
        let next = current.next in
        if next = None then false else aux (Option.get next)
    in
    aux (Option.get q.first)

let print_deque (to_str : 'a -> string) ({ first; _ } : 'a t) : unit =
  let rec aux = function
    | None -> Printf.printf "None\n"
    | Some a ->
        Printf.printf "%s;" (to_str a.value);
        aux a.next
  in
  aux first

let print_deque_from_last (to_str : 'a -> string) ({ last; _ } : 'a t) : unit =
  let rec aux = function
    | None -> Printf.printf "\n"
    | Some a ->
        Printf.printf "%s;" (to_str a.value);
        aux a.prev
  in
  Printf.printf "None;";
  aux last

let length { first; _ } =
  let rec aux = function None -> 0 | Some a -> 1 + aux a.next in
  aux first
