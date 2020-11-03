type span =
  { left: int;
    right: int;
  }

let merge_span { left; _ } { right; _} = { left; right }

type ctx =
  { path:    string;
    in_chan: in_channel;
  }
