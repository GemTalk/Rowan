private
serializeJson: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString