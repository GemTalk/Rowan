accessing
pathFor: coll
  | ans sz |
  ans := WriteStreamPortable on: String new.
  sz := coll size.
  1 to: sz do: [ :index | 
    | pathElem |
    pathElem := coll at: index.
    ans nextPutAll: pathElem.
    index < sz
      ifTrue: [ ans nextPut: $/ ] ].
  ^ ans contents