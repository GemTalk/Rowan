private
materialize: string
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    next