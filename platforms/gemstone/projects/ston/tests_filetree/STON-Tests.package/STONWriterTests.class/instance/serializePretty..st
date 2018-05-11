private
serializePretty: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self prettyWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString