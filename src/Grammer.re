let byNumber = (count, word) => {
  count == 1 ? word : switch(word){
    | "is" => "are"
    | _ => word ++ "s"
  }
}
