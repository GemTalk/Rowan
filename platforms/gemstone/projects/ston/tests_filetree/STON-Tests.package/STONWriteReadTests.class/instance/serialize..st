private
serialize: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString