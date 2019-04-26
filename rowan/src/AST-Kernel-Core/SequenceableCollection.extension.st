Extension { #name : 'SequenceableCollection' }

{ #category : '*ast-kernel-core' }
SequenceableCollection >> rbStoreElementsFrom: firstIndex to: lastIndex on: aStream [
  | noneYet defaultElement arrayElement |
  noneYet := true.
  defaultElement := nil.
  firstIndex to: lastIndex do: [ :index | 
    arrayElement := self at: index.
    arrayElement = defaultElement
      ifFalse: [ 
        noneYet
          ifTrue: [ noneYet := false ]
          ifFalse: [ aStream nextPut: $; ].
        aStream nextPutAll: ' at: '.
        index rbStoreOn: aStream.
        aStream nextPutAll: ' put: '.
        arrayElement rbStoreOn: aStream] ].
  ^ noneYet

]

{ #category : '*ast-kernel-core' }
SequenceableCollection >> rbStoreOn: aStream [
  aStream nextPutAll: '(('.
  aStream nextPutAll: self class name.
  aStream nextPutAll: ' new: '.
  self size rbStoreOn: aStream.
  aStream nextPut: $).
  (self rbStoreElementsFrom: 1 to: self size on: aStream)
    ifFalse: [ aStream nextPutAll: '; yourself' ].
  aStream nextPut: $)

]
