printing
fullPath
  | ans |
  ans := WriteStreamPortable on: String new.
  path
    do: [ :pathElem | 
      ans nextPut: $/.
      ans nextPutAll: pathElem ].
  self query == nil
    ifFalse: [ 
      ans nextPut: $?.
      ans nextPutAll: self query ].
  self fragment == nil
    ifFalse: [ 
      ans nextPut: $#.
      ans nextPutAll: self fragment ].
  ^ ans contents