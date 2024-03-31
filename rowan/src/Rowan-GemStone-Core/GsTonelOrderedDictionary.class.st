"
I am an implementation of a dictionary. Compared to other dictionaries I am very efficient for small sizes, speed- and space-wise. I also mantain the order in which elements are added when iterating. My implementation features some ideas from the RefactoringBrowser.
"
Class {
	#name : 'GsTonelOrderedDictionary',
	#superclass : 'Object',
	#instVars : [
		'size',
		'keys',
		'values'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
GsTonelOrderedDictionary class >> new [
	^ self new: 3

]

{ #category : 'instance creation' }
GsTonelOrderedDictionary class >> new: anInteger [
	^ self basicNew initialize: anInteger; yourself

]

{ #category : 'instance creation' }
GsTonelOrderedDictionary class >> withAll: aDictionary [
	^ (self new: aDictionary size)
		addAll: aDictionary;
		yourself

]

{ #category : 'filetree' }
GsTonelOrderedDictionary >> _writeCypressJsonOn: fileStream [
	"Private method which may be removed in a future GemStone version."

	self _writeCypressJsonOn: fileStream indent: 0
]

{ #category : 'filetree' }
GsTonelOrderedDictionary >> _writeCypressJsonOn: aStream indent: startIndent [
	"Private method which may be removed in a future GemStone version."

	| indent cnt |
	indent := startIndent.
	aStream nextPutAll: '{'.
	cnt := 0.
	indent := indent + 1.
	self keys do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			aStream lf.
			indent timesRepeat: [aStream tab].
			key _writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value _writeCypressJsonOn: aStream indent: indent.
			cnt < size
				ifTrue: 
					[aStream nextPutAll: ',' ]].
	self size > 0
		ifTrue: [ aStream lf ]
		ifFalse: [ aStream space ].
	aStream nextPutAll: ' }'
]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> add: anAssociation [
	self at: anAssociation key put: anAssociation value.
	^ anAssociation

]

{ #category : 'adding' }
GsTonelOrderedDictionary >> addAll: aDictionary [
	aDictionary keysAndValuesDo: [ :key :value | self at: key put: value ].
	^ aDictionary

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> associationsDo: aBlock [
	self keysAndValuesDo: [ :key :value | aBlock value: key -> value ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> at: aKey [
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> at: aKey ifAbsent: aBlock [
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> at: aKey ifAbsentPut: aBlock [
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> at: aKey ifPresent: aBlock [
	"Lookup aKey in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0 ifFalse: [ aBlock value: (values at: index) ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> at: aKey put: aValue [
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> do: aBlock [
	1 to: size do: [ :index | aBlock value: (values at: index) ]

]

{ #category : 'private' }
GsTonelOrderedDictionary >> errorKeyNotFound [
	self error: 'Key not found'

]

{ #category : 'private' }
GsTonelOrderedDictionary >> findIndexFor: aKey [
	1 to: size do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0

]

{ #category : 'private' }
GsTonelOrderedDictionary >> grow [
	| newKeys newValues |
	newKeys := Array new: 2 * size.
	newValues := Array new: 2 * size.
	1 to: size do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues

]

{ #category : 'testing' }
GsTonelOrderedDictionary >> includesKey: aKey [
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0

]

{ #category : 'initialization' }
GsTonelOrderedDictionary >> initialize: anInteger [
  size := 0.
  keys := Array new: anInteger.
  values := Array new: anInteger

]

{ #category : 'testing' }
GsTonelOrderedDictionary >> isCollection [
	^ true

]

{ #category : 'testing' }
GsTonelOrderedDictionary >> isEmpty [
	^ size == 0

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> keys [
	^ keys copyFrom: 1 to: size

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> keysAndValuesDo: aBlock [
	1 to: size do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> keysDo: aBlock [
	1 to: size do: [ :each | aBlock value: (keys at: each) ]

]

{ #category : 'copying' }
GsTonelOrderedDictionary >> postCopy [
	super postCopy.
	keys := keys copy.
	values := values copy

]

{ #category : 'printing' }
GsTonelOrderedDictionary >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	self size <= 100
		ifTrue: [
			| first |
			first := true.
			self keysAndValuesDo: [ :key :value |
				"keysAndValuesDo:separatedBy: would be nice"
				first
					ifTrue: [ first := false ]
					ifFalse: [ aStream space ].
				aStream
					print: key;
					nextPutAll: '->';				
					print: value ] ]
		ifFalse: [
			aStream
				nextPutAll: 'size ';
				print: self size ].
	aStream nextPut: $)	

]

{ #category : 'private' }
GsTonelOrderedDictionary >> privateAt: aKey put: aValue [
	size = keys size ifTrue: [ self grow ].
	keys at: (size := size + 1) put: aKey.
	^ values at: size put: aValue

]

{ #category : 'private' }
GsTonelOrderedDictionary >> removeIndex: index [
	| value |
	value := values at: index.
	index to: size - 1 do:
			[ :i | 
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1) ].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^ value

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> removeKey: aKey [
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> removeKey: aKey ifAbsent: aBlock [
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	^ self removeIndex: index

]

{ #category : 'accessing' }
GsTonelOrderedDictionary >> size [
	^ size

]

{ #category : 'ston' }
GsTonelOrderedDictionary >> stonOn: stonWriter [
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
    stonWriter encodeMap: self

]

{ #category : 'enumerating' }
GsTonelOrderedDictionary >> values [
	^ values copyFrom: 1 to: size

]
