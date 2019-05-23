printing
printOn: aStream
  aStream
    nextPutAll: self schemeName;
    nextPut: $:.
  aStream nextPutAll: '//'.
  aStream nextPutAll: self pathString