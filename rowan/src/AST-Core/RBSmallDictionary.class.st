"
RBSmallDictionary is a special dictionary optimized for small collections. In addition to the normal dictionary protocol, it also supports an #empty message which ""empties"" the collection but may hang on to the original elements (so it could collect garbage). Without #empty we would either need to create a new dictionary or explicitly remove everything from the dictionary. Both of these take more time and #empty.

Instance Variables:
array <Array of: Object> array of keys (we don't use Associations for our key value pairs)
tally <Integer> the size of the dictionary
values <Array of: Object> array of our values

"
Class {
	#name : 'RBSmallDictionary',
	#superclass : 'Object',
	#instVars : [
		'keys',
		'values',
		'tally'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBSmallDictionary class >> new [
	^ self new: 2
]

{ #category : 'instance creation' }
RBSmallDictionary class >> new: anInteger [ 
	^ self basicNew initialize: anInteger
]

{ #category : 'accessing' }
RBSmallDictionary >> at: aKey [
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]
]

{ #category : 'accessing' }
RBSmallDictionary >> at: aKey ifAbsent: aBlock [
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]
]

{ #category : 'accessing' }
RBSmallDictionary >> at: aKey ifAbsentPut: aBlock [
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]
]

{ #category : 'accessing' }
RBSmallDictionary >> at: aKey put: aValue [
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]
]

{ #category : 'accessing' }
RBSmallDictionary >> empty [
	tally := 0
]

{ #category : 'private' }
RBSmallDictionary >> errorKeyNotFound [
	self error: 'Key not found'
]

{ #category : 'private' }
RBSmallDictionary >> findIndexFor: aKey [
	1 to: tally do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0
]

{ #category : 'private' }
RBSmallDictionary >> grow [
	| newKeys newValues |
	newKeys := Array new: 2 * tally.
	newValues := Array new: 2 * tally.
	1 to: tally do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues
]

{ #category : 'testing' }
RBSmallDictionary >> includesKey: aKey [
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0
]

{ #category : 'initialization' }
RBSmallDictionary >> initialize: anInteger [
	tally := 0.
	keys := Array new: anInteger.
	values := Array new: anInteger
]

{ #category : 'testing' }
RBSmallDictionary >> isEmpty [
	^ tally == 0
]

{ #category : 'accessing' }
RBSmallDictionary >> keys [
	^ keys copyFrom: 1 to: tally
]

{ #category : 'enumerating' }
RBSmallDictionary >> keysAndValuesDo: aBlock [
	1 to: tally do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]
]

{ #category : 'enumerating' }
RBSmallDictionary >> keysDo: aBlock [
	1 to: tally do: [ :each | aBlock value: (keys at: each) ]
]

{ #category : 'copying' }
RBSmallDictionary >> postCopy [
	super postCopy.
	keys := keys copy.
	values := values copy
]

{ #category : 'private' }
RBSmallDictionary >> privateAt: aKey put: aValue [
	tally = keys size ifTrue: [ self grow ].
	keys at: (tally := tally + 1) put: aKey.
	^ values at: tally put: aValue
]

{ #category : 'removing' }
RBSmallDictionary >> removeKey: aKey [
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]
]

{ #category : 'removing' }
RBSmallDictionary >> removeKey: aKey ifAbsent: aBlock [
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index value |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	value := values at: index.
	index to: tally - 1 do: [ :i |
		keys at: i put: (keys at: i + 1).
		values at: i put: (values at: i + 1) ].
	keys at: tally put: nil.
	values at: tally put: nil.
	tally := tally - 1.
	^ value
]

{ #category : 'accessing' }
RBSmallDictionary >> size [
	^ tally
]

{ #category : 'accessing' }
RBSmallDictionary >> values [
	^ values copyFrom: 1 to: tally
]

{ #category : 'enumerating' }
RBSmallDictionary >> valuesDo: aBlock [
	1 to: tally do: [ :index | aBlock value: (values at: index) ]
]
