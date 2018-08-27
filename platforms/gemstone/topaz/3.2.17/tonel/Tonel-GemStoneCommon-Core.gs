! Package: Tonel-GemStoneCommon-Core


! Remove existing behavior from package Tonel-GemStoneCommon-Core
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Tonel-GemStoneCommon-Core'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(Object
	subclass: 'GsTonelOrderedDictionary'
	instVarNames: #( size keys values )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-GemStoneCommon-Core';
		comment: 'I am an implementation of a dictionary. Compared to other dictionaries I am very efficient for small sizes, speed- and space-wise. I also mantain the order in which elements are added when iterating. My implementation features some ideas from the RefactoringBrowser.';
		immediateInvariant.
true.
%

! Class Implementation for GsTonelOrderedDictionary

! ------------------- Class methods for GsTonelOrderedDictionary

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new
	^ self new: 3
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new: anInteger
	^ self basicNew initialize: anInteger; yourself
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
withAll: aDictionary
	^ (self new: aDictionary size)
		addAll: aDictionary;
		yourself
%

! ------------------- Instance methods for GsTonelOrderedDictionary

category: 'accessing'
method: GsTonelOrderedDictionary
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
%

category: 'adding'
method: GsTonelOrderedDictionary
addAll: aDictionary
	aDictionary keysAndValuesDo: [ :key :value | self at: key put: value ].
	^ aDictionary
%

category: 'accessing'
method: GsTonelOrderedDictionary
associations
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents
%

category: 'enumerating'
method: GsTonelOrderedDictionary
associationsDo: aBlock
	self keysAndValuesDo: [ :key :value | aBlock value: key -> value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsent: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsentPut: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifPresent: aBlock
	"Lookup aKey in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0 ifFalse: [ aBlock value: (values at: index) ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey put: aValue
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
do: aBlock
	1 to: size do: [ :index | aBlock value: (values at: index) ]
%

category: 'private'
method: GsTonelOrderedDictionary
errorKeyNotFound
	self error: 'Key not found'
%

category: 'private'
method: GsTonelOrderedDictionary
findIndexFor: aKey
	1 to: size do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0
%

category: 'private'
method: GsTonelOrderedDictionary
grow
	| newKeys newValues |
	newKeys := Array new: 2 * size.
	newValues := Array new: 2 * size.
	1 to: size do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues
%

category: 'testing'
method: GsTonelOrderedDictionary
includesKey: aKey
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0
%

category: 'initialization'
method: GsTonelOrderedDictionary
initialize: anInteger
  size := 0.
  keys := Array new: anInteger.
  values := Array new: anInteger
%

category: 'testing'
method: GsTonelOrderedDictionary
isCollection
	^ true
%

category: 'testing'
method: GsTonelOrderedDictionary
isEmpty
	^ size = 0
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keys
	^ keys copyFrom: 1 to: size
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysAndValuesDo: aBlock
	1 to: size do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysDo: aBlock
	1 to: size do: [ :each | aBlock value: (keys at: each) ]
%

category: 'copying'
method: GsTonelOrderedDictionary
postCopy
	super postCopy.
	keys := keys copy.
	values := values copy
%

category: 'printing'
method: GsTonelOrderedDictionary
printOn: aStream
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
%

category: 'private'
method: GsTonelOrderedDictionary
privateAt: aKey put: aValue
	size = keys size ifTrue: [ self grow ].
	keys at: (size := size + 1) put: aKey.
	^ values at: size put: aValue
%

category: 'private'
method: GsTonelOrderedDictionary
removeIndex: index
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
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey ifAbsent: aBlock
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	^ self removeIndex: index
%

category: 'accessing'
method: GsTonelOrderedDictionary
size
	^ size
%

category: 'ston'
method: GsTonelOrderedDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
    stonWriter encodeMap: self
%

category: 'enumerating'
method: GsTonelOrderedDictionary
values
	^ values copyFrom: 1 to: size
%

! Class Extensions

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| cr lf start sz nextLF nextCR |
	start := 1.
	sz := self size.
	cr := Character cr.
	nextCR := self indexOf: cr startingAt: 1.
	lf := Character lf.
	nextLF := self indexOf: lf startingAt: 1.
	[ start <= sz ] whileTrue: [
		(nextLF = 0 and: [ nextCR = 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR = 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR = nextLF
				ifTrue: [ "Found a CR-LF pair"
					aBlock value: start value: nextCR - 1 value: nextLF.
					start := 1 + nextLF.
					nextCR := self indexOf: cr startingAt: start.
					nextLF := self indexOf: lf startingAt: start ]
				ifFalse: [ "Found a CR"
					aBlock value: start value: nextCR - 1 value: nextCR.
					start := 1 + nextCR.
					nextCR := self indexOf: cr startingAt: start ]]]
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
withLineEndings: lineEndingString

	| stream |
	
	stream := nil.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		(stream isNil and: [ endWithoutDelimiters ~= end ]) ifTrue: [
			(self copyFrom: endWithoutDelimiters + 1 to: end) = lineEndingString ifFalse: [
				stream := WriteStreamPortable with: self copy.
				stream position: start - 1 ]].
		stream ifNotNil: [
			stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
			endWithoutDelimiters = end ifFalse: [
				stream nextPutAll: lineEndingString ]]].
	^stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ]]
%

! Class Extension for Collection

! ------------------- Instance methods for Collection

category: '*tonel-gemstonecommon-core'
method: Collection
asDictionary

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattened
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	^ Array streamContents: [ :stream | self flattenOn: stream].
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattenOn: aStream

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].
%

! Class Extension for GsFile

! ------------------- Instance methods for GsFile

category: '*tonel-gemstonecommon-core'
method: GsFile
<< items

 	items putOn: self.
	
	^ self
%

! Class Extension for PositionableStreamPortable

! ------------------- Instance methods for PositionableStreamPortable

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
match: subCollection
  "Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

  | pattern startMatch |
  pattern := ReadStreamPortable on: subCollection.
  startMatch := nil.
  [ pattern atEnd ]
    whileFalse: [ 
      self atEnd
        ifTrue: [ ^ false ].
      self next = pattern next
        ifTrue: [ 
          pattern position = 1
            ifTrue: [ startMatch := self position ] ]
        ifFalse: [ 
          pattern position: 0.
          startMatch
            ifNotNil: [ 
              self position: startMatch.
              startMatch := nil ] ] ].
  ^ true
%

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
%

! Class Extension for SequenceableCollection

! ------------------- Instance methods for SequenceableCollection

category: '*tonel-gemstonecommon-core'
method: SequenceableCollection
pairsCollect: aBlock 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
%

! Class Extension for TonelParser

! ------------------- Class methods for TonelParser

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
readStreamClass

	^ ReadStreamPortable
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
writeStreamClass

	^ WriteStreamPortable
%

! Class Extension for TonelWriter

! ------------------- Class methods for TonelWriter

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
lineEnding
  "Answer the os-specific line endings"

  ^ String with: Character lf
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
orderedDictionaryClass
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
%

! Class initializers 

doit
true.
%



! End of Package: Tonel-GemStoneCommon-Core


