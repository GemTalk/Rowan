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
%


! Class Declarations

! Class Extensions

! Class Extension for OrderedCollection

! ------------------- Class methods for OrderedCollection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: OrderedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: OrderedCollection
streamSpecies

	^Array
%

! Class Extension for ByteArray

! ------------------- Instance methods for ByteArray

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
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

! Class Extension for GsNMethod

! ------------------- Instance methods for GsNMethod

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: GsNMethod
category

	^self inClass categoryOfSelector: self selector
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: GsNMethod
methodClass

	^self inClass
%

! Class Extension for GsFile

! ------------------- Instance methods for GsFile

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: GsFile
tab: anInteger

	anInteger timesRepeat: [self tab]
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: GsFile
upToEnd

	^self next: (self fileSize - self positionA)
%

! Class Extension for Character

! ------------------- Instance methods for Character

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Character
isSafeForHTTP
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^self codePoint < 128
		and: [self isAlphaNumeric or: ['.-_' includes: self]]
%

! Class Extension for SequenceableCollection

! ------------------- Class methods for SequenceableCollection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: SequenceableCollection
new: newSize streamContents: aOneArgBlock

	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	aOneArgBlock value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: SequenceableCollection
streamContents: aOneArgBlock

	^ self new: 100 streamContents: aOneArgBlock
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: SequenceableCollection
streamSpecies
	"Answer the class that is used for streaming.
	 If overridden, consider overriding #new:streamContents:."

	^self
%

! ------------------- Instance methods for SequenceableCollection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SequenceableCollection
beginsWith: aSequence
	"Answer whether the first elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence startingAt: 1) = 1
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer a copy of the receiver."

	^self copyWithoutSuffix: aSequence or: [self copy].
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence or: aBlock
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer the result of evaluating aBlock."

	(self endsWith: aSequence) ifFalse: [^aBlock value].
	^self copyFrom: 1 to: self size - aSequence size.
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SequenceableCollection
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	| expectedStart |
	expectedStart := self size - aSequence size + 1 max: 1.
	^expectedStart
		= (self indexOfSubCollection: aSequence startingAt: expectedStart)
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
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

! Class Extension for Symbol

! ------------------- Class methods for Symbol

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: Symbol
new: size streamContents: aOneArgBlock

	^(super new: size streamContents: aOneArgBlock) asSymbol
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: Symbol
streamSpecies

	^String
%

! Class Extension for SortedCollection

! ------------------- Class methods for SortedCollection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: SortedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: SortedCollection
streamSpecies

	^Array
%

! Class Extension for DateAndTimeANSI

! ------------------- Class methods for DateAndTimeANSI

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: DateAndTimeANSI
fromUnixFormatString: aString
	"YYYY-MM-DDTHH:MM:SS +HHMM
	 Examples:
		| string |
		string := '2013-06-20 14:47:55.40271592140198 -0700'.
		(DateAndTimeANSI fromUnixFormatString: string) printString = '2013-06-20T14:47:55.40271592140198-07:00'.
	"

	| stream sign positionBias |
	stream := ReadStream on: aString.
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

! Class Extension for Behavior

! ------------------- Instance methods for Behavior

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Behavior
methodDictionary

	^self methodDictForEnv: 0
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Behavior
parseSelectorFrom: methodString

	| meth |
	meth := self
				_parseMethod: methodString
				category: #'xyzzy'
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	^meth selector asString
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
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

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean
	"If a receiver contains subString beginning at some point at or after
	 startIndex, this returns the index at which subString begins.  If the
	 receiver does not contain subString, this returns 0."

	^self
		_findString: subString
		startingAt: startIndex
		ignoreCase: aBoolean not
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: CharacterCollection
withUnixLineEndings
	"Assume the string is textual, and that CR, LF, and CRLF are all valid line endings.
	 Replace each occurence with a single LF."

	| cr lf inPos outPos outString newOutPos indexLF indexCR |
	cr := Character cr.
	indexCR := self indexOf: cr startingAt: 1.
	indexCR = 0 ifTrue: [^self].
	lf := Character lf.
	indexLF := self indexOf: lf startingAt: 1.
	indexLF = 0 ifTrue: [^self copyReplacing: cr with: lf].
	inPos := outPos := 1.
	outString := String new: self size.
	
	["check if next CR is before next LF or if there are no more LF"
	(indexLF = 0 or: [indexCR < indexLF])
		ifTrue: 
			[newOutPos := outPos + 1 + indexCR - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.
			1 + indexCR = indexLF
				ifTrue: 
					["Caught a CR-LF pair"
					inPos := 1 + indexLF.
					indexLF := self indexOf: lf startingAt: inPos]
				ifFalse: [inPos := 1 + indexCR].
			indexCR := self indexOf: cr startingAt: inPos]
		ifFalse: 
			[newOutPos := outPos + 1 + indexLF - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 1
				with: self
				startingAt: inPos.
			outPos := newOutPos.
			inPos := 1 + indexLF.
			indexLF := self indexOf: lf startingAt: inPos].
	indexCR = 0]
			whileFalse.

	"no more CR line endings. copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.
	^outString copyFrom: 1 to: newOutPos - 1
%

! Class Extension for SymbolList

! ------------------- Instance methods for SymbolList

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: SymbolList
allSatisfying: aOneArgBlock
	"Answer the elements from the receiver's Symbol Dictionaries
	 which meet the criteria specified in aOneArgBlock."

	| result |
	result := Array new.
	self asArray do: [:each | result addAll: (each select: aOneArgBlock)].
	^result.
%

! Class Extension for String

! ------------------- Instance methods for String

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
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

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: String
unescapePercents
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped char asciiVal specialChars oldPos pos |
	unescaped := ReadWriteStream on: String new.
	specialChars := '+%' asSet.
	oldPos := 1.
	
	[pos := self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0]
			whileTrue: 
				[unescaped nextPutAll: (self copyFrom: oldPos to: pos - 1).
				char := self at: pos.
				(char = $% and: [pos + 2 <= self size])
					ifTrue: 
						[asciiVal := ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
									+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
						asciiVal > 255 ifTrue: [^self].
						unescaped nextPut: (Character withValue: asciiVal).
						pos := pos + 3.
						pos <= self size ifFalse: [char := nil].
						oldPos := pos]
					ifFalse: 
						[char = $+
							ifTrue: [unescaped nextPut: Character space]
							ifFalse: [unescaped nextPut: char].
						oldPos := pos + 1]].
	oldPos <= self size
		ifTrue: [unescaped nextPutAll: (self copyFrom: oldPos to: self size)].
	^unescaped contents
%

! Class Extension for Interval

! ------------------- Class methods for Interval

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
classmethod: Interval
streamSpecies

	^Array
%

! Class Extension for Collection

! ------------------- Instance methods for Collection

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Collection
difference: aCollection
	"Answer the set theoretic difference of two collections."

	| set |
	set := self asSet.
	aCollection do: [:each | set remove: each ifAbsent: []].
	^self species withAll: set asArray
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Collection
gather: aBlock

	^Array
		streamContents: [:stream | self do: [:ea | stream nextPutAll: (aBlock value: ea)]]
%

category: '*Cypress-GemStoneCompatibilityLayer'
set compile_env: 0
method: Collection
intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	| set outputSet |
	set := self asSet.
	outputSet := Set new.
	aCollection do: 
			[:each |
			((set includes: each) and: [(outputSet includes: each) not])
				ifTrue: [outputSet add: each]].
	^self species withAll: outputSet asArray
%

! Class initializers 

doit
%



! End of Package: Cypress-GemStoneCompatibilityLayer


