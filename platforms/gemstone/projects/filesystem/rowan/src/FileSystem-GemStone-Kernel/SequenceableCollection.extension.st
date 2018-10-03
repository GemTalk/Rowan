Extension { #name : 'SequenceableCollection' }

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> allButFirst [
  "Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

  ^ self allButFirst: 1
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> allButFirst: n [
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyAfterLast: anElement [
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyLast: n [
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyUpToLast: anElement [
  "Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	| n |
	n :=  (self lastIndexOf: anElement ifAbsent: [ ^ self copy ]) - 1.
  ^ self copyFrom: 1 to: n

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyWithFirst: newElement [ 
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species new: self size + 1.
	newIC 
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> grownBy: length [
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species new: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> lastIndexOf: anElement ifAbsent: exceptionBlock [
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  ^ self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock

]

{ #category : '*filesystem-gemstone-kernel' }
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

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> readStreamPortable [

	^ ReadStreamPortable on: self

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> replaceAll: oldObject with: newObject [
  "Replace all occurences of oldObject with newObject"

  | index |
  index := self indexOf: oldObject startingAt: 1 ifAbsent: [ 0 ].
  [ index = 0 ]
    whileFalse: [ 
      self at: index put: newObject.
      index := self indexOf: oldObject startingAt: index + 1 ifAbsent: [ 0 ] ]
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> reversed [
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self species new: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result

]
