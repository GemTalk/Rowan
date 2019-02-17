Extension { #name : 'Object' }

{ #category : '*filesystem-gemstone-kernel' }
Object >> assert: aBlock [
	"Throw an assertion error if aBlock does not evaluates to true.
	We check for true explicitly to make the assertion fail for non booleans"
	self assert: aBlock description: 'Assertion failed'.
]

{ #category : '*filesystem-gemstone-kernel' }
Object >> assert: aBlockOrBoolean description: aStringOrBlock [
	"Throw an assertion error if aBlock does not evaluates to true."
	
	(aBlockOrBoolean isKindOf: BlockClosure) 
		ifTrue: [
			aBlockOrBoolean value ifFalse: [ AssertionFailure signal: aStringOrBlock value ] ]
		ifFalse: [
			aBlockOrBoolean ifFalse: [ AssertionFailure signal: aStringOrBlock ] ]
]

{ #category : '*filesystem-gemstone-kernel' }
Object >> flag: aSymbol [

	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
]

{ #category : '*filesystem-gemstone-kernel' }
Object >> isCharacter [

	^ false

]

{ #category : '*filesystem-gemstone-kernel' }
Object >> split: aSequenceableCollection [
	"Split the argument using the receiver as a separator."
	"optimized version for single delimiters"
	"($/ split: '/foo/bar')>>>#('' 'foo' 'bar') asOrderedCollection"
	"([:c| c isSeparator] split: 'aa bb cc dd')>>> #('aa' 'bb' 'cc' 'dd') asOrderedCollection"
		
	| result |
	result := OrderedCollection new: (aSequenceableCollection size / 2) asInteger.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result

]

{ #category : '*filesystem-gemstone-kernel' }
Object >> split: aSequenceableCollection do: aBlock [
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :item | ]"
	self split: aSequenceableCollection indicesDo: [ :start :end | 
		aBlock value: (aSequenceableCollection copyFrom: start to: end) ]

]

{ #category : '*filesystem-gemstone-kernel' }
Object >> split: aSequenceableCollection indicesDo: aBlock [
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. optimized version for single delimiters."
	
	"(String streamContents: [:s | Character space split: 'Pharo is cool'  indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"
		
		
		
	|  position oldPosition |
	
	position := 1.
	oldPosition := position.
	
	position := aSequenceableCollection indexOf: self startingAt: position.
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + 1.
		oldPosition := position.
		position := aSequenceableCollection indexOf: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.

]
