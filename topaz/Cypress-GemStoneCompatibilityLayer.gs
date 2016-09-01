! Package: Cypress-GemStoneCompatibilityLayer


! Remove existing behavior from package Cypress-GemStoneCompatibilityLayer
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-GemStoneCompatibilityLayer'.
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
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

! Class Extensions

! Class Extension for Behavior

! ------------------- Instance methods for Behavior

category: '*Cypress-GemStoneCompatibilityLayer'
method: Behavior
_parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError."

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[self
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
	       intoMethodDict: GsMethodDictionary new
	       intoCategories: GsMethodDictionary new  
	       intoPragmas: nil
		environmentId: anEnvironmentId]
			onSynchronous: (Array with: CompileError with: CompileWarning)
			do: (Array with: 
						[:ex |
						| undefSymbol symbols |
						undefSymbol := true.
						symbols := Array new.
						ex errorDetails do: 
								[:errArray |
								(errArray atOrNil: 1) == 1031
									ifTrue: [symbols add: (errArray atOrNil: 5) asSymbol]
									ifFalse: [undefSymbol := false]].
						undefSymbol
							ifTrue: 
								["attempt auto-define of undefined symbols"
								symbols do: [:sym | undefinedSymbols at: sym put: nil].
								
								[^self
									compileMethod: source
									dictionaries: aSymbolList , undefinedSymbolList
									category: cat
								       intoMethodDict: GsMethodDictionary new
								       intoCategories: GsMethodDictionary new
								       intoPragmas: nil
									environmentId: anEnvironmentId]
										onException: CompileError
										do: [:exb | undefSymbol := false]].
						undefSymbol ifFalse: [ex outer]]
					with: [:ex | ex resume])
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: Behavior
methodDictionary

	^self methodDictForEnv: 0
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: Behavior
parseSelectorFrom: methodString

	| meth |
	^[meth := self
				_parseMethod: methodString
				category: #'xyzzy'
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	meth selector asString]
		on: CompileError
		do: [:ex | ex return: '_____could_not_parse_selector_from_method_source_____'].
%

! Class Extension for ByteArray

! ------------------- Instance methods for ByteArray

category: '*Cypress-GemStoneCompatibilityLayer'
method: ByteArray
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:each |
					| c |
					(c := Character withValue: each) isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							each // 16 printOn: stream base: 16 showRadix: false.
							each \\ 16 printOn: stream base: 16 showRadix: false]]]
%

! Class Extension for Character

! ------------------- Instance methods for Character

category: '*squeak'
method: Character
isSafeForHTTP
	"whether a character is 'safe', or needs to be escaped when used, eg, in a URL"

	^ self charCode < 128
		and: [ self isAlphaNumeric
				or: [ '.-_' includes: self ]]
%

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

category: '*squeak'
method: CharacterCollection
encodeForHTTP
	"change dangerous characters to their %XX form, for use in HTTP transactions"
	| encodedStream |
	encodedStream _ WriteStream on: (String new).
	
	1 to: self size do: [ :n | | c |
		c := self at: n.
		c isSafeForHTTP ifTrue: [ encodedStream nextPut: c ] ifFalse: [
			encodedStream nextPut: $%.
			encodedStream nextPut: (c asciiValue // 16) asHexDigit.
			encodedStream nextPut: (c asciiValue \\ 16) asHexDigit.
		]
	].
	^encodedStream contents.
%

category: '*squeak'
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not
%

category: '*monticellofiletree-core'
method: CharacterCollection
withUnixLineEndings

	^self withGemstoneLineEndings
%

! Class Extension for Collection

! ------------------- Instance methods for Collection

category: '*squeak-enumerating'
method: Collection
difference: aCollection
	"Answer the set theoretic difference of two collections."

	^ self reject: [:each | aCollection includes: each]
%

category: '*packageinfo-base'
method: Collection
gather: aBlock
  | res |
  res := { } .
  self do: [:ea | res addAll: (aBlock value: ea)].
  ^ res
%

category: '*squeak'
method: Collection
intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	^ self select: [:each | aCollection includes: each]
%

! Class Extension for DateAndTimeANSI

! ------------------- Class methods for DateAndTimeANSI

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: DateAndTimeANSI
fromUnixFormatString: aString
	"YYYY-MM-DDTHH:MM:SS +HHMM
	 Examples:
		| string |
		string := '2013-06-20 14:47:55.40271592140198 -0700'.
		(DateAndTimeANSI fromUnixFormatString: string) printString = '2013-06-20T14:47:55.40271592140198-07:00'.
	"

	| stream sign positionBias |
	stream := ReadStreamPortable on: aString.
	sign := aString at: aString size - 4.
	positionBias := stream class isLegacyStreamImplementation
		ifTrue: [1]
		ifFalse: [0].
	^self
		year:   (stream next: 4) asNumber
		month:  (stream next; next: 2) asNumber
		day:    (stream next; next: 2) asNumber
		hour:   (stream next; next: 2) asNumber
		minute: (stream next; next: 2) asNumber
		second: (stream next; next: (aString size - 6 - stream position + positionBias)) asNumber
		offset: (Duration
			days:    0 
			hours:   (stream next; next; next: 2) asNumber * (sign == $- ifTrue: [-1] ifFalse: [1])
			minutes: (stream next: 2) asNumber
			seconds: 0)
%

! Class Extension for GsFile

! ------------------- Instance methods for GsFile

category: '*squeak'
method: GsFile
tab: anInteger 
	"Append anInteger tab characters to the receiver."

	anInteger timesRepeat: [self tab]
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: GsFile
upToEnd

	^self next: (self fileSize - self positionA)
%

! Class Extension for GsNMethod

! ------------------- Instance methods for GsNMethod

category: '*Cypress-GemStoneCompatibilityLayer'
method: GsNMethod
category

	^self inClass categoryOfSelector: self selector
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: GsNMethod
methodClass

	^self inClass
%

! Class Extension for Interval

! ------------------- Class methods for Interval

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: Interval
streamSpecies

	^Array
%

! Class Extension for OrderedCollection

! ------------------- Class methods for OrderedCollection

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: OrderedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: OrderedCollection
streamSpecies

	^Array
%

! Class Extension for SequenceableCollection

! ------------------- Class methods for SequenceableCollection

category: '*core-squeak'
classmethod: SequenceableCollection
new: newSize streamContents: blockWithArg

	| stream |
	stream := WriteStream on: (self new: newSize).
	blockWithArg value: stream.
	stream positionA = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]
%

category: '*core-squeak'
classmethod: SequenceableCollection
streamContents: blockWithArg

	^self new: 100 streamContents: blockWithArg
%

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: SequenceableCollection
streamSpecies
	"Answer the class that is used for streaming.
	 If overridden, consider overriding #new:streamContents:."

	^self
%

! ------------------- Instance methods for SequenceableCollection

category: '*core-squeak'
method: SequenceableCollection
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
%

category: '*core-squeak'
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true
%

category: '*squeak-copying'
method: SequenceableCollection
copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])
%

category: '*squeak'
method: SequenceableCollection
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])
%

category: '*squeak-copying'
method: SequenceableCollection
copyUpTo: anObject
  "Answer all elements up to but not including anObject. If there
  is no such object, answer a copy of the receiver."

| idx |
idx := self indexOf: anObject startingAt: 1 .
idx == 0 ifTrue:[ 
  ^ self copy 
] ifFalse:[
  ^ self copyFrom: 1 to: idx - 1
]
%

category: '*squeak'
method: SequenceableCollection
copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: SequenceableCollection
copyWithoutSuffix: aSequence
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer a copy of the receiver."

	^self copyWithoutSuffix: aSequence or: [self copy].
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: SequenceableCollection
copyWithoutSuffix: aSequence or: aBlock
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer the result of evaluating aBlock."

	(self endsWith: aSequence) ifFalse: [^aBlock value].
	^self copyFrom: 1 to: self size - aSequence size.
%

category: '*squeak'
method: SequenceableCollection
endsWith: aSequenceableCollection

	| start |
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
%

category: '*Cypress-GemStoneCompatibilityLayer'
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.
	Note: it is user responsibility to provide aCollection that behaves relatevily fast when asked for includes: (like a Set)"

	start to: self size do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value
%

category: '*squeak'
method: SequenceableCollection
lastIndexOf: anElement
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: nil
%

category: '*squeak'
method: SequenceableCollection
lastIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
%

category: '*squeak'
method: SequenceableCollection
lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock ~~ nil ifTrue:[ exceptionBlock value ] ifFalse:[ 0 ]
%

! Class Extension for SortedCollection

! ------------------- Class methods for SortedCollection

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: SortedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: SortedCollection
streamSpecies

	^Array
%

! Class Extension for String

! ------------------- Instance methods for String

category: '*Cypress-GemStoneCompatibilityLayer'
method: String
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:c |
					c isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							c codePoint // 16 printOn: stream base: 16 showRadix: false.
							c codePoint \\ 16 printOn: stream base: 16 showRadix: false]]]
%

category: '*gssqueakcommon-core'
method: String
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP.
	 Assume UTF8 encoding for Pharo compatibility"

	"this method not implemented in CharacterCollection in order to preserve original behavior without overrides"

	^self unescapePercentsWithTextEncoding: 'utf-8'
%

! Class Extension for Symbol

! ------------------- Class methods for Symbol

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: Symbol
new: size streamContents: aOneArgBlock

	^(super new: size streamContents: aOneArgBlock) asSymbol
%

category: '*Cypress-GemStoneCompatibilityLayer'
classmethod: Symbol
streamSpecies

	^String
%

! Class Extension for SymbolList

! ------------------- Instance methods for SymbolList

category: '*Cypress-GemStoneCompatibilityLayer'
method: SymbolList
allSatisfying: aOneArgBlock
	"Answer the elements from the receiver's Symbol Dictionaries
	 which meet the criteria specified in aOneArgBlock."

	| result |
	result := Array new.
	self asArray do: [:each | result addAll: (each select: aOneArgBlock)].
	^result.
%

! Class initializers 

doit
true.
%



! End of Package: Cypress-GemStoneCompatibilityLayer


