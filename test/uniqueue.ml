type 'a t = 'a Unique.Once.t Queue.t

let create () = Queue.create ()
let push v t = Queue.push (Unique.Once.make v) t
let pop t = Unique.Once.take_exn (Queue.pop t)
let is_empty t = Queue.is_empty t
