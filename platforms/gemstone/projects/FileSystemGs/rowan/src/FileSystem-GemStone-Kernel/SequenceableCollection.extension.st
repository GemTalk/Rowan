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
SequenceableCollection >> atRandom: aGenerator [
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (aGenerator nextInt: self size)
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyAfter: anElement [
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyAfterLast: anElement [
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> copyFrom: start to: stop [ 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start
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
SequenceableCollection >> copyUpThrough: anElement [
    "Answer all elements up to and including anObject. If there
     is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
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
SequenceableCollection >> endsWith: aSequenceableCollection [
	"Answer true if the receiver ends with the argument collection"
	
	| start |
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true
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
SequenceableCollection >> removeAll [

	self removeFrom: 1 to: self size

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

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> swap: oneIndex with: anotherIndex [ 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> withIndexCollect: elementAndIndexBlock [ 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (
			elementAndIndexBlock
				value: (self at: index)
				value: index)].
	^ newCollection

]

{ #category : '*filesystem-gemstone-kernel' }
SequenceableCollection >> writeStream [
	^ WriteStream on: self
]
