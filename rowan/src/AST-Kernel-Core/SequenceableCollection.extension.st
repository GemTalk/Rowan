Extension { #name : 'SequenceableCollection' }

{ #category : '*ast-kernel-core' }
SequenceableCollection >> lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock [
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  lastIndex to: 1 by: -1 do: [ :index | 
    (self at: index) = anElement
      ifTrue: [ ^ index ] ].
  ^ exceptionBlock ~~ nil
    ifTrue: [ exceptionBlock value ]
    ifFalse: [ 0 ]

]

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

{ #category : '*ast-kernel-core' }
SequenceableCollection >> readStreamPortable [

	^ ReadStreamPortable on: self

]
