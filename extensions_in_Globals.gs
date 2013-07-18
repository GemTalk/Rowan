












! On July 17, 2013, 4:47:01 PM
doit
(Object subclass: 'SymbolDictionaryComparer'  instVarNames: #( userSymbolDictionaryMapping symbolDictionaries targetDirectoryPath                    fileName summaryReportStream addedMethods changedMethods                    deletedMethods addedClasses deletedClasses)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: Globals  options: #())
.
%

! ------------------- Class comment for SymbolDictionaryComparer
doit
SymbolDictionaryComparer comment: 
'Examples:

SymbolDictionaryComparer forAllUsers report
SymbolDictionaryComparer forGlobalsOnly report
SymbolDictionaryComparer forAllUsers reportInto: ''/home/rsargent/2013-02-15_13.14.54''; summaryReport

SymbolDictionaryComparer forGlobalsOnly reportInto: ''/home/rsargent/2013-02-15_13.14.54''; summaryReport
'
%
doit
(Object subclass: 'SymbolDictionaryDumper'  instVarNames: #( userSymbolDictionaryMapping symbolDictionaries targetDirectoryPath                    fileName)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: Globals  options: #())
.
%

! ------------------- Class comment for SymbolDictionaryDumper
doit
SymbolDictionaryDumper comment: 
'Examples:

SymbolDictionaryDumper forAllUsers report
SymbolDictionaryDumper forGlobalsOnly report
SymbolDictionaryDumper forAllUsers reportIntoTimestampedDirectoryUnder: ''/home/rsargent''

SymbolDictionaryDumper forGlobalsOnly reportIntoTimestampedDirectoryUnder: ''/home/rsargent''
'
%
doit
(SymbolDictionaryDumper subclass: 'SymbolDictionarySnapshotter'  instVarNames: #( snapshot)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: Globals  options: #())
.
%

! ------------------- Class comment for SymbolDictionarySnapshotter
doit
SymbolDictionarySnapshotter comment: 
'Examples:

SymbolDictionarySnapshotter forAllUsers publicSnapshot
SymbolDictionarySnapshotter forGlobalsOnly temporarySnapshot
'
%
doit
(Object subclass: 'SymbolDictionarySnapshotComparer'  instVarNames: #( olderSnapshot newerSnapshot olderDiffs                    newerDiffs)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: Globals  options: #())
.
%

! ------------------- Class comment for SymbolDictionarySnapshotComparer
doit
SymbolDictionarySnapshotComparer comment: 
''
%
doit
(Object subclass: 'SymbolDictionarySnapshotManager'  instVarNames: #( repository)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: Globals  options: #())
.
%

! ------------------- Class comment for SymbolDictionarySnapshotManager
doit
SymbolDictionarySnapshotManager comment: 
''
%
doit

SymbolDictionaryComparer immediateInvariant.
%
doit
SymbolDictionaryDumper immediateInvariant.
%
doit
SymbolDictionarySnapshotter immediateInvariant.
%
doit
SymbolDictionarySnapshotComparer immediateInvariant.
%
doit
SymbolDictionarySnapshotManager immediateInvariant.
%

category: '*Cypress-Mocks-Extensions'
set compile_env: 0
method: Object
isCypressMockBasic

	^false
%
category: '*Cypress-Structure'
set compile_env: 0
method: Object
asCypressPropertyObject

	^self
%

category: '*Cypress-Structure'
set compile_env: 0
method: Behavior
methodDictionary

	^self methodDictForEnv: 0
%

category: '*Cypress-Definitions'
set compile_env: 0
method: Class
asCypressClassDefinition

	^CypressClassDefinition
		name: self name
		superclassName: self superclass name
		category: self category
		instVarNames: self instVarNames
		classInstVarNames: self class instVarNames
		classVarNames: self classVarNames
		comment: self comment.
%

category: '*Cypress-Structure'
set compile_env: 0
method: Boolean
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString
%

category: '*Cypress-Definitions'
set compile_env: 0
method: Collection
difference: aCollection
	"Answer the set theoretic difference of two collections."

	| set |
	set := self asSet.
	aCollection do: [:each | set remove: each ifAbsent: []].
	^self species withAll: set asArray
%
category: '*Cypress-Definitions'
set compile_env: 0
method: Collection
gather: aBlock

	^Array
		streamContents: [:stream | self do: [:ea | stream nextPutAll: (aBlock value: ea)]]
%
category: '*Cypress-Definitions'
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

category: '*Cypress-Structure'
set compile_env: 0
method: Dictionary
asCypressPropertyObject

	| result |
	result := self class new: self size.
	self associationsDo: [:assoc | result at: assoc key put: assoc value asCypressPropertyObject].
	^result.
%
category: '*Cypress-Structure'
set compile_env: 0
method: Dictionary
writeCypressJsonOn: aStream indent: startIndent

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys asSortedCollection do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			aStream tab: indent.
			key writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' }'
%

category: '*Cypress-Structure'
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
category: '*Cypress-Structure'
set compile_env: 0
classmethod: SequenceableCollection
streamContents: aOneArgBlock

	^ self new: 100 streamContents: aOneArgBlock
%
category: '*Cypress-Structure'
set compile_env: 0
classmethod: SequenceableCollection
streamSpecies
	"Answer the class that is used for streaming.
	 If overridden, consider overriding #new:streamContents:."

	^self
%

category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
beginsWith: aSequence
	"Answer whether the first elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence startingAt: 1) = 1
%
category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer a copy of the receiver."

	^self copyWithoutSuffix: aSequence or: [self copy].
%
category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
copyWithoutSuffix: aSequence or: aBlock
	"Answer a copy of the receiver excluding the specified suffix.
	 If the suffix does not match, answer the result of evaluating aBlock."

	(self endsWith: aSequence) ifFalse: [^aBlock value].
	^self copyFrom: 1 to: self size - aSequence size.
%
category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
endsWith: aSequence
	"Answer whether the last elements of the receiver are the same as aSequence."

	^(self indexOfSubCollection: aSequence
		startingAt: (self size - aSequence size max: 1))
			= (self size - aSequence size + 1)
%
category: '*Cypress-Structure'
set compile_env: 0
method: SequenceableCollection
indexOfAnyOf: aCollection startingAt: start
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]
%
category: '*Cypress-Structure'
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

category: '*Cypress-Structure'
set compile_env: 0
method: Array
asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject]
%
category: '*Cypress-Structure'
set compile_env: 0
method: Array
writeCypressJsonOn: aStream indent: startIndent

	| indent |
	aStream
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			aStream tab: indent.
			item writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' ]'
%

category: '*Cypress-Definitions'
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

category: '*Cypress-Structure'
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

category: '*Cypress-Definitions'
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
category: '*Cypress-Definitions'
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

category: '*Cypress-Structure'
set compile_env: 0
method: String
asCypressPropertyObject

	^self unescapePercents withUnixLineEndings
%
category: '*Cypress-Structure'
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
category: '*Cypress-Structure'
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
category: '*Cypress-Structure'
set compile_env: 0
method: String
writeCypressJsonOn: aStream indent: startIndent

	aStream
		nextPutAll: '"';
		nextPutAll: self withUnixLineEndings encodeAsUTF8 escapePercents;
		nextPutAll: '"'
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: Symbol
new: size streamContents: aOneArgBlock

	^(super new: size streamContents: aOneArgBlock) asSymbol
%
category: '*Cypress-Structure'
set compile_env: 0
classmethod: Symbol
streamSpecies

	^String
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: Interval
streamSpecies

	^Array
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: OrderedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%
category: '*Cypress-Structure'
set compile_env: 0
classmethod: OrderedCollection
streamSpecies

	^Array
%

category: '*Cypress-Structure'
set compile_env: 0
classmethod: SortedCollection
new: size streamContents: aOneArgBlock

	^self withAll: (super new: size streamContents: aOneArgBlock)
%
category: '*Cypress-Structure'
set compile_env: 0
classmethod: SortedCollection
streamSpecies

	^Array
%

category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
asCypressMethodDefinition

	^CypressMethodDefinition
		className: self methodClass theNonMetaClass name
		classIsMeta: self methodClass isMeta
		selector: self selector
		category: self category
		source: self sourceString
%
category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
category

	^self inClass categoryOfSelector: self selector
%
category: '*Cypress-Definitions'
set compile_env: 0
method: GsNMethod
methodClass

	^self inClass
%

category: '*Cypress-Structure'
set compile_env: 0
method: Character
isSafeForHTTP
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^self codePoint < 128
		and: [self isAlphaNumeric or: ['.-_' includes: self]]
%
category: 'other'
set compile_env: 0
classmethod: DateAndTimeANSI
fromString: aString

	"YYYY-MM-DDTHH:MM:SS+HH:MM

	 Examples:

		| string |

		string := '2013-06-20T14:47:55.40271592140198-07:00'.

		(DateAndTimeANSI fromString: string) printString = string.



		| time |

		time := DateAndTime now.

		(DateAndTime fromString: time printString) = time.

	"



	| stream sign positionBias |

	stream := ReadStream on: aString.

	sign := aString at: aString size - 5.

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

			hours:   (stream next; next: 2) asNumber * (sign == $- ifTrue: [-1] ifFalse: [1])

			minutes: (stream next; next: 2) asNumber

			seconds: 0)

%
category: '*Cypress-Structure'
set compile_env: 0
method: Number
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString
%

! Remove existing behavior from SymbolDictionaryComparer
doit
SymbolDictionaryComparer removeAllMethods.
SymbolDictionaryComparer class removeAllMethods.
%
! ------------------- Class methods for SymbolDictionaryComparer
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryComparer
forAllUsers

	^self new
		addSymbolListsForAllUsers;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryComparer
forGlobalsOnly

	^self new
		addGlobals;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryComparer
new

	^super new
		initialize;
		yourself
%
! ------------------- Instance methods for SymbolDictionaryComparer
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryComparer
fileName

   ^fileName
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryComparer
summaryReport

	^self summaryReportStream contents
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryComparer
symbolDictionaries

   ^symbolDictionaries
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryComparer
targetDirectoryPath

   ^targetDirectoryPath
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
addedClasses

	^addedClasses
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
addedMethods

	^addedMethods
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
changedMethods

	^changedMethods
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
deletedClasses

	^deletedClasses
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
deletedMethods

	^deletedMethods
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
fileNameFor: aSymbolDictionary andUserId: aString

	^self targetDirectoryPath, '/',
		aString, '-', aSymbolDictionary name, '-', self fileName
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
summaryReportStream

	^summaryReportStream
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryComparer
userSymbolDictionaryMapping

   ^userSymbolDictionaryMapping
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initialize

	self
		initializeUserSymbolDictionaryMapping;
		initializeSymbolDictionaries;
		initializeTargetDirectoryPath;
		initializeFileName;
		initializeSummaryReportStream
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeChanges

	self
		addedMethods: OrderedCollection new;
		changedMethods: OrderedCollection new;
		deletedMethods: OrderedCollection new;
		addedClasses: OrderedCollection new;
		deletedClasses: OrderedCollection new
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeFileName

	self fileName: ((System gemVersionReport at: #gsRelease) copyReplaceAll: ' ' with: ''), '-sourceStrings.obj'.

%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeSummaryReportStream

	self summaryReportStream: (WriteStream on: String new)

%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeSymbolDictionaries

	self symbolDictionaries: IdentitySet new
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeTargetDirectoryPath

	self targetDirectoryPath: '/tmp'
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryComparer
initializeUserSymbolDictionaryMapping

	self userSymbolDictionaryMapping: Dictionary new
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryComparer
report

	self userSymbolDictionaryMapping keysAndValuesDo: 
			[:userId :symbolDicts |
			symbolDicts
				do: [:each | self compareSymbolDictionary: each forUserId: userId]]
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryComparer
reportInto: aString

	self
		targetDirectoryPath: aString;
		report
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
compareDeletedMethods: meths fromClass: theClass named: clsNam withDesignator: clsMethStr filingOutTo: fileinFile

	| envPrefix prefixSize lf |
	envPrefix := ':env_'.
	prefixSize := envPrefix size.
	lf := Character lf.
	meths keysDo: 
			[:aName |
			| envId namSiz selectr |
			envId := 0.
			selectr := aName.
			(aName at: 1 equals: envPrefix)
				ifTrue: 
					[| endIdx |
					endIdx := aName indexOf: $: startingAt: 2.
					envId := Integer
								fromString: (aName copyFrom: prefixSize + 1 to: endIdx - 1).
					selectr := aName copyFrom: endIdx + 1 to: aName size].
			(theClass includesSelector: selectr environmentId: envId)
				ifFalse: 
					[self deletedMethods add: clsNam , '>>' , clsMethStr , aName.
					fileinFile
						nextPutAll: 'doit' , lf , clsNam , ' removeSelector: #' , selectr
								, ' environmentId:' , envId asString
								, lf , $%
								, lf]]
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
compareDeletionsIn: aSymbolDictionary from: prevClasses filingOutOn: fileinFile
	"Now check to see if there are any deleted methods or classes"

	prevClasses keysAndValuesDo: 
			[:clsNam :meths |
			((aSymbolDictionary includesKey: clsNam) and: [(aSymbolDictionary at: clsNam) isBehavior])
				ifFalse: [self deletedClasses add: clsNam]
				ifTrue: 
					[| theClass |
					theClass := aSymbolDictionary at: clsNam.
					self
						compareDeletedMethods: (meths at: 1)
						fromClass: theClass
						named: clsNam
						withDesignator: ''
						filingOutTo: fileinFile.
					self
						compareDeletedMethods: (meths at: 2)
						fromClass: theClass class
						named: clsNam
						withDesignator: '(C)'
						filingOutTo: fileinFile]]
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
compareMethods: mDict fromClass: theClass withDesignator: clsMethStr against: prevDict filingOutTo: fileinFile

	mDict keysAndValuesDo: 
			[:selector :aMeth |
			| envId aName |
			envId := aMeth environmentId.
			aName := envId == 0
						ifTrue: [selector]
						ifFalse: [':env_' , envId asString , ':' , selector asString].
			" check to see if same method exists in prevClasses"
			(prevDict includesKey: aName)
				ifFalse: 
					[self addedMethods add: theClass nameForFileout , '>>' , clsMethStr , aName.
					fileinFile nextPutAll: (theClass fileOutMethod: selector environmentId: envId)]
				ifTrue: 
					[" method exists in both versions, do a diff."
					aMeth sourceString = (prevDict at: aName)
						ifFalse: 
							[self changedMethods add: theClass nameForFileout , '>>' , clsMethStr , aName.
							fileinFile nextPutAll: (theClass fileOutMethod: selector environmentId: envId)]]]
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
compareMethodSourcesIn: aSymbolDictionary from: prevClasses filingOutOn: fileinFile
	"Iterate through the classes in the current SymbolDictionary and compare method source strings."

	aSymbolDictionary do: 
			[:ea |
			ea isBehavior
				ifTrue: 
					[| meths classMeths className |
					className := ea nameForFileout.
					(prevClasses includesKey: className)
						ifFalse: 
							[self addedClasses add: className.
							ea fileOutClassOn: fileinFile]
						ifTrue: 
							[ea persistentMethodDictsDo: 
									[:mDict |
									self
										compareMethods: mDict
										fromClass: ea
										withDesignator: ''
										against: ((prevClasses at: className) at: 1)
										filingOutTo: fileinFile].
							ea class persistentMethodDictsDo: 
									[:mDict |
									self
										compareMethods: mDict
										fromClass: ea class
										withDesignator: '(C)'
										against: ((prevClasses at: className) at: 2)
										filingOutTo: fileinFile]]]].
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
compareSymbolDictionary: aSymbolDictionary forUserId: aString
	"Compile a report on Smalltalk method differences between this version and a previous version. 
	 The report string will be written to a file with the same name plus the suffix '.changes.report'.
	 The changes are also written into a file with the same name plus the suffix '.changes.filein' in Topaz filein format."

	| prevClasses fileinFile fName |
	prevClasses := self
				readPreviouslySavedClassesInformationFor: aSymbolDictionary
				andUserId: aString.
	fileinFile := self openFileinFileFor: aSymbolDictionary andUserId: aString.
	self
		initializeChanges;
		compareMethodSourcesIn: aSymbolDictionary from: prevClasses filingOutOn: fileinFile;
		compareDeletionsIn: aSymbolDictionary from: prevClasses filingOutOn: fileinFile;
		reportChangesIn: aSymbolDictionary forUserId: aString.
	fileinFile close
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
openFileinFileFor: aSymbolDictionary andUserId: aString

	| fileinFile fName |
	fileinFile := GsFile
				openWriteOnServer: (fName := (self fileNameFor: aSymbolDictionary
								andUserId: aString) , '.changes.filein').
	fileinFile == nil ifTrue: [self error: 'Can''t open ' , fName , ' for writing.'].
	^fileinFile
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
readPreviouslySavedClassesInformationFor: aSymbolDictionary andUserId: aString
	"Read in and answer the prevClasses from passivated object file corresponding to the given SymbolDictionary and User Id."

	| prevClasses prevClassesFile fName |
	prevClassesFile := GsFile
				openReadOnServer: (fName := self fileNameFor: aSymbolDictionary
								andUserId: aString).
	prevClassesFile == nil ifTrue: [self error: 'Can''t open ' , fName , ' for reading.'].

	"read in the prevClasses from passivated object file"
	prevClasses := (PassiveObject newOnStream: prevClassesFile) activate.
	prevClasses == nil
		ifTrue: [self error: 'Failed to activate source dictionaries from ' , fName , '.'].
	^prevClasses
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryComparer
reportChangesIn: aSymbolDictionary forUserId: aString
	"Put together the report."

	| reportFile fName lf |
	lf := Character lf.
	self summaryReportStream
		nextPutAll: '====' , lf , 'Image change report ', aString, ' ', aSymbolDictionary name , lf , '====' , lf.
	self addedClasses size ~~ 0
		ifTrue: 
			[self summaryReportStream
				nextPutAll: '----' , lf , 'Added Classes:' , lf , '----' , lf.
			self addedClasses sortAscending
				do: [:ea | self summaryReportStream nextPutAll: ea , lf]].
	self deletedClasses size ~~ 0
		ifTrue: 
			[self summaryReportStream
				nextPutAll: '----' , lf , 'Deleted Classes:' , lf , '----' , lf.
			self deletedClasses sortAscending
				do: [:ea | self summaryReportStream nextPutAll: ea , lf]].
	self addedMethods size ~~ 0
		ifTrue: 
			[self summaryReportStream
				nextPutAll: '----' , lf , 'Added Methods:' , lf , '----' , lf.
			self addedMethods sortAscending
				do: [:ea | self summaryReportStream nextPutAll: ea , lf]].
	self deletedMethods size ~~ 0
		ifTrue: 
			[self summaryReportStream
				nextPutAll: '----' , lf , 'Deleted Methods' , lf , '----' , lf.
			self deletedMethods sortAscending
				do: [:ea | self summaryReportStream nextPutAll: ea , lf]].
	self changedMethods size ~~ 0
		ifTrue: 
			[self summaryReportStream
				nextPutAll: '----' , lf , 'Changed Methods' , lf , '----' , lf.
			self changedMethods sortAscending
				do: [:ea | self summaryReportStream nextPutAll: ea , lf]].
	reportFile := GsFile
				openWriteOnServer: (fName := (self fileNameFor: aSymbolDictionary
								andUserId: aString) , '.changes.report').
	reportFile == nil
		ifTrue: [self error: 'Can''t open ' , fName , ' for writing.'].
	reportFile
		nextPutAll: self summaryReport;
		close
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryComparer
add: aSymbolDictionary forUserId: aString

	((self symbolDictionaries includes: aSymbolDictionary)
		or: [aSymbolDictionary _behaviorKeys isEmpty]) ifTrue: [^self].
	self symbolDictionaries add: aSymbolDictionary.
	(self userSymbolDictionaryMapping at: aString ifAbsentPut: [OrderedCollection new])
		add: aSymbolDictionary
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryComparer
addGlobals

	self add: Globals forUserId: 'SystemUser'
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryComparer
addSymbolListsForAllUsers

	AllUsers do: 
			[:user |
			user symbolList
				do: [:symbolDict | self add: symbolDict forUserId: user userId]]
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryComparer
fileName: aString

   fileName := aString
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryComparer
targetDirectoryPath: aString

   targetDirectoryPath := aString
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
addedClasses: someStrings

	addedClasses := someStrings
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
addedMethods: someStrings

	addedMethods := someStrings
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
changedMethods: someStrings

	changedMethods := someStrings
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
deletedClasses: someStrings

	deletedClasses := someStrings
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
deletedMethods: someStrings

	deletedMethods := someStrings
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
summaryReportStream: aWriteStream

	summaryReportStream := aWriteStream
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
symbolDictionaries: someSymbolDictionaries

   symbolDictionaries := someSymbolDictionaries
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryComparer
userSymbolDictionaryMapping: someSymbolDictionariesKeyedByUserId

   userSymbolDictionaryMapping := someSymbolDictionariesKeyedByUserId
%

! Remove existing behavior from SymbolDictionaryDumper
doit
SymbolDictionaryDumper removeAllMethods.
SymbolDictionaryDumper class removeAllMethods.
%
! ------------------- Class methods for SymbolDictionaryDumper
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryDumper
forAllUsers

	^self new
		addSymbolListsForAllUsers;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryDumper
forGlobalsOnly

	^self new
		addGlobals;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryDumper
forMe

	^self new
		addMySymbolList;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryDumper
forUserId: aString

	^self new
		addSymbolListForUserId: aString;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionaryDumper
new

	^super new
		initialize;
		yourself
%
! ------------------- Instance methods for SymbolDictionaryDumper
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryDumper
fileName

   ^fileName
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryDumper
symbolDictionaries

   ^symbolDictionaries
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryDumper
targetDirectoryPath

   ^targetDirectoryPath
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionaryDumper
userSymbolDictionaryMapping

   ^userSymbolDictionaryMapping
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionaryDumper
fileNameFor: aSymbolDictionary andUserId: aString

	^self targetDirectoryPath, '/',
		aString, '-', aSymbolDictionary name, '-', self fileName
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryDumper
initialize

	self
		initializeUserSymbolDictionaryMapping;
		initializeSymbolDictionaries;
		initializeTargetDirectoryPath;
		initializeFileName
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryDumper
initializeFileName

	self fileName: ((System gemVersionReport at: #gsRelease) copyReplaceAll: ' ' with: ''), '-sourceStrings.obj'.

%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryDumper
initializeSymbolDictionaries

	self symbolDictionaries: IdentitySet new
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryDumper
initializeTargetDirectoryPath

	self targetDirectoryPath: '/tmp'
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionaryDumper
initializeUserSymbolDictionaryMapping

	self userSymbolDictionaryMapping: Dictionary new
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryDumper
report

	self userSymbolDictionaryMapping keysAndValuesDo: 
			[:userId :symbolDicts |
			symbolDicts
				do: [:each | self reportSymbolDictionary: each forUserId: userId]]
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryDumper
reportInto: aString

	self
		targetDirectoryPath: aString;
		report
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryDumper
reportIntoTimestampedDirectoryUnder: aString

	| fullPath |
	fullPath := aString, '/', self timestampDirectoryName.
	(GsFile createServerDirectory: fullPath) isNil ifTrue: [self error: GsFile lastErrorString].
	self reportInto: fullPath
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionaryDumper
timestampDirectoryName

	| in out insertion |
	in := ReadStream on: DateAndTime now printString.
	out := WriteStream on: (String new: 19).
	out nextPutAll: (in next: 10).
	insertion := $_.
	3 timesRepeat: 
			[in next.
			out nextPut: insertion.
			out nextPutAll: (in next: 2).
			insertion := $.].
	^out contents
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryDumper
compileReportOfSymbolDictionary: aSymbolDictionary forUserId: aString
	"Generate a Dictionary data structure containing all the source code for the classes
	 in the specified Symbol Dictionary. The Dictionary produced by this method can be used by 
	 to generate a report and/or a filein script to reconcile class differences between two versions
	 of the image, or any number of other things."

	| classes |
	classes := Dictionary new.
	(aSymbolDictionary select: [:each | each isKindOf: Behavior])
		do: [:each | classes at: each nameForFileout put: (self flattenClassDetailsFor: each)].
	^classes
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryDumper
flattenClassDetailsFor: aClass

	| classDetails |
	classDetails := (Dictionary new)
				at: 'class category' put: aClass category;
				at: 'class comment' put: aClass comment;
				at: 'class definition' put: aClass definition;
				at: 'instance methods' put: Dictionary new;
				at: 'class methods' put: Dictionary new;
				yourself.
	self
		flattenMethodDictionaryFor: aClass
			into: (classDetails at: 'instance methods');
		flattenMethodDictionaryFor: aClass class
			into: (classDetails at: 'class methods').
	^classDetails
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryDumper
flattenMethodDictionaryFor: aClass into: targetDict

	aClass persistentMethodDictsDo: 
			[:mDict |
			mDict keysAndValuesDo: 
					[:k :aMeth |
					| envId aName |
					envId := aMeth environmentId.
					aName := envId == 0
								ifTrue: [k]
								ifFalse: [k asString , ':env_' , envId asString].
					targetDict at: aName
						put: (aClass categoryOfSelector: aName) -> aMeth sourceString]]
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionaryDumper
reportSymbolDictionary: aSymbolDictionary forUserId: aString
	"Generate a passivated data structure containing all the source code for the classes
	 in the specified Symbol Dictionary. The file generated by this method can be used by 
	 to generate a report and/or filein script to reconcile class differences between two versions
	 of the image."

	| classes outfile outfilename |
	classes := self compileReportOfSymbolDictionary: aSymbolDictionary forUserId: aString.
	outfilename := self fileNameFor: aSymbolDictionary andUserId: aString.
	outfile := GsFile openWriteOnServer: outfilename.
	outfile == nil ifTrue: [^'error opening outfile' , outfilename, ' (', GsFile lastErrorString asString, ')'].
	(PassiveObject passivate: classes toStream: outfile) == nil
		ifTrue: ['error writing outfile' , outfilename, ' (', GsFile lastErrorString asString, ')'].
	outfile close.
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
add: aSymbolDictionary forUserId: aString

	((self symbolDictionaries includes: aSymbolDictionary)
		or: [aSymbolDictionary _behaviorKeys isEmpty]) ifTrue: [^self].
	self symbolDictionaries add: aSymbolDictionary.
	(self userSymbolDictionaryMapping at: aString ifAbsentPut: [OrderedCollection new])
		add: aSymbolDictionary
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
addGlobals

	self add: Globals forUserId: 'SystemUser'
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
addMySymbolList

	self addSymbolListForUserProfile: System myUserProfile.
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
addSymbolListForUserId: aString

	self addSymbolListForUserProfile: (System users detect: [:each | each userId = aString]).
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
addSymbolListsForAllUsers

	AllUsers do: 
			[:user |
			user symbolList
				do: [:symbolDict | self add: symbolDict forUserId: user userId]]
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
fileName: aString

   fileName := aString
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionaryDumper
targetDirectoryPath: aString

   targetDirectoryPath := aString
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryDumper
addSymbolListForUserProfile: aUserProfile

	aUserProfile symbolList do: [:each | self add: each forUserId: aUserProfile userId]
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryDumper
symbolDictionaries: someSymbolDictionaries

   symbolDictionaries := someSymbolDictionaries
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionaryDumper
userSymbolDictionaryMapping: someSymbolDictionariesKeyedByUserId

   userSymbolDictionaryMapping := someSymbolDictionariesKeyedByUserId
%

! Remove existing behavior from SymbolDictionarySnapshotter
doit
SymbolDictionarySnapshotter removeAllMethods.
SymbolDictionarySnapshotter class removeAllMethods.
%
! ------------------- Class methods for SymbolDictionarySnapshotter
! ------------------- Instance methods for SymbolDictionarySnapshotter
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotter
snapshot

   ^snapshot
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
snapshotDescription

	^self snapshot at: 'Description' ifAbsent: ['']
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
initialize

	super initialize.
	self initializeSnapshot
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
initializeSnapshot

	self snapshot: Dictionary new
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
privateSnapshot
	"Take a snapshot, saving it UserGlobals.
	 Answer the snapshot."

	^self
		report;
		saveInUserGlobals;
		snapshot
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
publicSnapshot
	"Take a snapshot, saving it Globals.
	 Answer the snapshot."

	^self
		report;
		saveInGlobals;
		snapshot
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
report

	self recordTimesWhile: [super report]
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
reportInto: aString
	"Snapshots are saved to the database, not the file system."

	self shouldNotImplement: #reportInto:
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
reportIntoTimestampedDirectoryUnder: aString
	"Snapshots are saved to the database, not the file system."

	self shouldNotImplement: #reportIntoTimestampedDirectoryUnder:
%
category: 'Reporting'
set compile_env: 0
method: SymbolDictionarySnapshotter
temporarySnapshot
	"Take a snapshot without saving it anywhere.
	 Answer the snapshot."

	^self
		report;
		snapshot
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
reportSymbolDictionary: aSymbolDictionary forUserId: aString

	self record: (self compileReportOfSymbolDictionary: aSymbolDictionary forUserId: aString)
		as: aSymbolDictionary name , '-' , aString
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
saveInGlobals

	self saveSnapshotIn: Globals
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
saveInUserGlobals

	self saveSnapshotIn: UserGlobals
%
category: 'Reporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
saveSnapshotIn: aSymbolDictionary

	(aSymbolDictionary at: #'Source Code Snapshots' ifAbsentPut: [Dictionary new])
		at: (self snapshot at: 'Finished at')
		put: self snapshot.
	System commit
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotter
add: aSymbolDictionary forUserId: aString

	super add: aSymbolDictionary forUserId: aString.
	self uniqueDescription: aSymbolDictionary name, '-', aString
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotter
addGlobals

	super addGlobals.
	self uniqueDescription: 'Globals only'.
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotter
addSymbolListsForAllUsers
	"We need to record the user id when snapshotting everything,
	 as the class definitions report whose SymbolDictionaries the class is in.
	 This means some classes will report as (class not in your dictionaries) for
	 different users requesting the snapshot."

	super addSymbolListsForAllUsers.
	self snapshotDescription: 'Everything-', System myUserProfile userId.
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
addSymbolListForUserProfile: aUserProfile

	super addSymbolListForUserProfile: aUserProfile.
	self snapshotDescription: aUserProfile userId, '''s SymbolList'.
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
describeSnapshotAsFromVariousSources

	self snapshotDescription: '(various)'
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
record: anObject as: aString

	self snapshot at: aString put: anObject
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
recordTimesWhile: aBlock

	| start end elapsed |
	self record: (start := DateAndTime now) as: 'Started at'.
	aBlock value.
	self record: (end := DateAndTime now) as: 'Finished at'.
	self record: (elapsed := end - start) as: 'Elapsed time'.
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
snapshot: someFlattenedSymbolDictionaries

   snapshot := someFlattenedSymbolDictionaries
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
snapshotDescription: aString

	self record: aString as: 'Description'
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotter
uniqueDescription: aString

	self snapshotDescription isEmpty
		ifTrue: [self snapshotDescription: aString]
		ifFalse: [self describeSnapshotAsFromVariousSources]
%

! Remove existing behavior from SymbolDictionarySnapshotComparer
doit
SymbolDictionarySnapshotComparer removeAllMethods.
SymbolDictionarySnapshotComparer class removeAllMethods.
%
! ------------------- Class methods for SymbolDictionarySnapshotComparer
category: 'Comparing'
set compile_env: 0
classmethod: SymbolDictionarySnapshotComparer
differencesBetween: oneSnapshot and: anotherSnapshot
	"Answer a Dictionary keyed by 'older' and 'newer' for the two snapshots
	 containing the subsets of each snapshot different from the other."

	^(self comparing: oneSnapshot and: anotherSnapshot) differences
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionarySnapshotComparer
comparing: oneSnapshot and: anotherSnapshot

	^self new
		initializeFrom: oneSnapshot and: anotherSnapshot;
		yourself
%
! ------------------- Instance methods for SymbolDictionarySnapshotComparer
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
ageOf: snapshot

	^snapshot at: 'Finished at'
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
categoryNotAvailable

	^'category not available' printString
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
commentNotAvailable

	^'comment not available' printString
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
definitionNotAvailable

	^'definition not available' printString
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
newerDiffs

	^newerDiffs
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
newerSnapshot

	^newerSnapshot
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
olderDiffs

	^olderDiffs
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
olderSnapshot

	^olderSnapshot
%
category: 'Comparing'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
differences
	"Answer a Dictionary keyed by 'older' and 'newer' for the two snapshots
	 containing the subsets of each snapshot different from the other."

	self compareSnapshots.
	^Dictionary new
		at: 'older' put: self olderDiffs;
		at: 'newer' put: self newerDiffs;
		yourself
%
category: 'Comparing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
compare: aSnapshotDictionary to: anotherSnapshotDictionary into: aDifferencesDictionary and: anotherDifferencesDictionary

	| combinedKeys |
	combinedKeys := (Set new)
				addAll: aSnapshotDictionary keys;
				addAll: anotherSnapshotDictionary keys;
				yourself.
	combinedKeys do: 
			[:each |
			self
				compareAtKey: each
				from: aSnapshotDictionary
				to: anotherSnapshotDictionary
				into: aDifferencesDictionary
				and: anotherDifferencesDictionary]
%
category: 'Comparing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
compareAtKey: key from: aSnapshotDictionary to: anotherSnapshotDictionary into: aDifferencesDictionary and: anotherDifferencesDictionary

	| aSubset anotherSubset aNestedDiffs anotherNestedDiffs |
	aSubset := aSnapshotDictionary at: key
				ifAbsent: [^anotherDifferencesDictionary at: key put: (anotherSnapshotDictionary at: key)].
	anotherSubset := anotherSnapshotDictionary at: key
				ifAbsent: [^aDifferencesDictionary at: key put: aSubset].
	aSubset class = Dictionary
		ifTrue: 
			[(aSubset includesKey: 'class definition')
				ifTrue: 
					[((aSubset at: 'class definition') = self definitionNotAvailable
						or: [(aSubset at: 'class comment') = self commentNotAvailable
						or: [(aSubset at: 'class category') = self categoryNotAvailable]])
						ifTrue: 
							[aSubset := aSubset deepCopy.
							(aSubset at: 'class definition') = self definitionNotAvailable
								ifTrue: [aSubset at: 'class definition' put: (anotherSubset at: 'class definition')].
							(aSubset at: 'class comment') = self commentNotAvailable
								ifTrue: [aSubset at: 'class comment' put: (anotherSubset at: 'class comment')].
							(aSubset at: 'class category') = self categoryNotAvailable
								ifTrue: [aSubset at: 'class category' put: (anotherSubset at: 'class category')]].
					((anotherSubset at: 'class definition') = self definitionNotAvailable
						or: [(anotherSubset at: 'class comment') = self commentNotAvailable
						or: [(anotherSubset at: 'class category') = self categoryNotAvailable]])
						ifTrue: 
							[anotherSubset := anotherSubset deepCopy.
							(anotherSubset at: 'class definition') = self definitionNotAvailable
								ifTrue: [anotherSubset at: 'class definition' put: (aSubset at: 'class definition')].
							(anotherSubset at: 'class comment') = self commentNotAvailable
								ifTrue: [anotherSubset at: 'class comment' put: (aSubset at: 'class comment')].
							(anotherSubset at: 'class category') = self categoryNotAvailable
								ifTrue: [anotherSubset at: 'class category' put: (aSubset at: 'class category')]]].
			aNestedDiffs := Dictionary new.
			anotherNestedDiffs := Dictionary new.
			self
				compare: aSubset
				to: anotherSubset
				into: aNestedDiffs
				and: anotherNestedDiffs.
			(aNestedDiffs notEmpty or: [anotherNestedDiffs notEmpty])
				ifTrue:
					[(aNestedDiffs notEmpty or: [aDifferencesDictionary == self olderDiffs])
						ifTrue: [aDifferencesDictionary at: key put: aNestedDiffs].
					(anotherNestedDiffs notEmpty or: [anotherDifferencesDictionary == self newerDiffs])
						ifTrue: [anotherDifferencesDictionary at: key put: anotherNestedDiffs]]]
		ifFalse: 
			["One of the snapshots might be imported from the old passivated classes code. Fix it up, if so."
			((aSubset isKindOf: Association)
				and: [anotherSubset isKindOf: Association])
					ifTrue: 
						[aSubset key = self categoryNotAvailable
							ifTrue: [aSubset := Association newWithKey: anotherSubset key value: aSubset value].
						anotherSubset key = self categoryNotAvailable
							ifTrue: [anotherSubset := Association newWithKey: aSubset key value: anotherSubset value]].
			aSubset = anotherSubset
				ifFalse: 
					[aDifferencesDictionary at: key put: aSubset.
					anotherDifferencesDictionary at: key put: anotherSubset]]
%
category: 'Comparing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
compareSnapshots

	self compare: self olderSnapshot to: self newerSnapshot into: self olderDiffs and: self newerDiffs
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
initializeFrom: oneSnapshot and: anotherSnapshot

	(self ageOf: oneSnapshot) > (self ageOf: anotherSnapshot)
		ifTrue: [^self initializeFrom: anotherSnapshot and: oneSnapshot].
	self
		olderSnapshot: oneSnapshot;
		newerSnapshot: anotherSnapshot;
		olderDiffs: Dictionary new;
		newerDiffs: Dictionary new
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
newerDiffs: aDictionary

	newerDiffs := aDictionary
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
newerSnapshot: aDictionary

	newerSnapshot := aDictionary
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
olderDiffs: aDictionary

	olderDiffs := aDictionary
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotComparer
olderSnapshot: aDictionary

	olderSnapshot := aDictionary
%

! Remove existing behavior from SymbolDictionarySnapshotManager
doit
SymbolDictionarySnapshotManager removeAllMethods.
SymbolDictionarySnapshotManager class removeAllMethods.
%
! ------------------- Class methods for SymbolDictionarySnapshotManager
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionarySnapshotManager
private

	^self new
		usePrivateRepository;
		yourself
%
category: 'Instance Creation'
set compile_env: 0
classmethod: SymbolDictionarySnapshotManager
public

	^self new
%
category: 'Instance Creation - private'
set compile_env: 0
classmethod: SymbolDictionarySnapshotManager
new

	^super new
		initialize;
		yourself
%
! ------------------- Instance methods for SymbolDictionarySnapshotManager
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
availableSnapshotDescriptions

	^(self repository values collect: [:each | self snapshotDescriptionOf: each]) sortDescending
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
availableSnapshotTimestamps

	^self repository keys sortAscending
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
latestSnapshot

	^self snapshotOf: self availableSnapshotTimestamps last
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
possibleSourcesForNewSnapshot

	^(Array with: 'Everything' with: 'My SymbolList')
		, (System users asArray collect: [:each | self possibleSourceForUserId: each userId])
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
privateSnapshotDescriptions

	^self
		usePrivateRepository;
		availableSnapshotDescriptions
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
publicSnapshotDescriptions

	^self
		usePublicRepository;
		availableSnapshotDescriptions
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
snapshotDescribedAs: aString

	^self snapshotOf: (self repositoryKeyFromDescription: aString)
%
category: 'Accessing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
snapshotOf: aDateAndTime
	"It is an error to request a snapshot that doesn't exist."

	^self repository at: aDateAndTime
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
descriptionForSnapshot: snapshot

	^(snapshot at: 'Description' ifAbsent: [''])
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
possibleSourceForUserId: aString

	^aString, '''s sources'
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
privateRepositorySource

	^UserGlobals
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
publicRepositorySource

	^Globals
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
repository

	^repository
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
repositoryKeyForSnapshot: snapshot

	^(snapshot at: 'Finished at')
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
repositoryKeyFromDescription: aString

	^(DateAndTime fromString: aString asArrayOfSubstrings first)
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
snapshotDescriptionOf: snapshot

	^(self repositoryKeyForSnapshot: snapshot) printString, ' ', (self descriptionForSnapshot: snapshot)
%
category: 'Accessing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
userProfileCorrespondingToSourceDescription: aString

	^System users
		detect: [:each | aString = (self possibleSourceForUserId: each userId)]
%
category: 'Comparing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
compareSnapshotDescribedAs: aSnapshotDescription against: alternateSnapshotDescription

	^self differencesBetween: (self snapshotDescribedAs: aSnapshotDescription)
		and: (self snapshotDescribedAs: alternateSnapshotDescription)
%
category: 'Comparing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
compareSnapshotDescribedAs: aSnapshotDescription againstLatestFromSourceMatching: aString

	^self differencesBetween: (self snapshotDescribedAs: aSnapshotDescription)
		and: (self snapshotFromSourceMatching: aString)
%
category: 'Comparing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
differencesBetween: oneSnapshot and: anotherSnapshot
	"Answer a Dictionary keyed by 'older' and 'newer' for the two snapshots
	 containing the subsets of each snapshot different from the other."

	^SymbolDictionarySnapshotComparer
		differencesBetween: oneSnapshot
		and: anotherSnapshot
%
category: 'Comparing'
set compile_env: 0
method: SymbolDictionarySnapshotManager
latestDifferencesForAllUsers

	^self differencesBetween: self latestSnapshot and: self allUsersSnapshot
%
category: 'Importing/Exporting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
exportSnapshotDescribedAs: aString toFileNamed: aPathName onClient: aBoolean

	| snapshot exportFile |
	snapshot := self snapshotOf: (self repositoryKeyFromDescription: aString).
	(exportFile := GsFile open: aPathName mode: 'w' onClient: aBoolean)
		ifNil: [self error: 'opening export' onFileNamed: aPathName].
	(PassiveObject passivate: snapshot toStream: exportFile)
		ifNil: [self error: 'writing export' onFileNamed: aPathName].
	exportFile close.
	^aPathName
%
category: 'Importing/Exporting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
importPassiveSourceFileFromFileNamed: aPathName onClient: aBoolean creationTimestamp: aDateAndTime

	| inportFile sources |
	(inportFile := GsFile open: aPathName mode: 'r' onClient: aBoolean)
		ifNil: [self error: 'opening import' onFileNamed: aPathName].
	sources := (PassiveObject newOnStream: inportFile) activate.
	sources ifNil: [self error: 'reading import' onFileNamed: aPathName].
	inportFile close.
	self commitSnapshot: (self convertPassiveSources: sources toSnapshotFormatDated: aDateAndTime).
%
category: 'Importing/Exporting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
importSnapshotFromFileNamed: aPathName onClient: aBoolean

	| inportFile snapshot |
	(inportFile := GsFile open: aPathName mode: 'r' onClient: aBoolean)
		ifNil: [self error: 'opening import' onFileNamed: aPathName].
	snapshot := (PassiveObject newOnStream: inportFile) activate.
	snapshot ifNil: [self error: 'reading import' onFileNamed: aPathName].
	inportFile close.
	self commitSnapshot: snapshot.
%
category: 'Importing/Exporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
convertPassiveSources: sources toSnapshotFormatDated: aDateAndTime

	| convertedSources |
	convertedSources := sources collect: [:each |
		Dictionary new
			at: 'class category' put: 'category not available' printString;
			at: 'class comment' put: 'comment not available' printString;
			at: 'class definition' put: 'definition not available' printString;
			at: 'instance methods' put: (each first collect: [:meth | 'category not available' printString -> meth]);
			at: 'class methods' put: (each last collect: [:meth | 'category not available' printString -> meth]);
			yourself
	].
	^Dictionary new
		at: 'Description' put: 'Passive sources from build';
		at: 'Finished at' put: aDateAndTime;
		at: 'Globals-SystemUser' put: convertedSources;
		yourself
%
category: 'Importing/Exporting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
error: aString onFileNamed: aPathName

	self error: 'error ', aString, ' file ', aPathName, ' (', GsFile lastErrorString asString, ')'
%
category: 'Initializing - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
initialize

	self usePublicRepository
%
category: 'Single Trip Operations'
set compile_env: 0
method: SymbolDictionarySnapshotManager
availableSnapshotDescriptionsAfterDeletingSnapshotDescribedAs: aString

	^self
		deleteSnapshotDescribedAs: aString;
		availableSnapshotDescriptions
%
category: 'Single Trip Operations'
set compile_env: 0
method: SymbolDictionarySnapshotManager
availableSnapshotDescriptionsAfterImportingSnapshotFromFileNamed: aPathName onClient: aBoolean

	^self
		importSnapshotFromFileNamed: aPathName onClient: aBoolean;
		availableSnapshotDescriptions
%
category: 'Single Trip Operations'
set compile_env: 0
method: SymbolDictionarySnapshotManager
availableSnapshotDescriptionsAfterTakingSnapshotFromSourceMatching: aString

	^self
		takeSnapshotFromSourceMatching: aString;
		availableSnapshotDescriptions
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
allUsersSnapshot

	^SymbolDictionarySnapshotter forAllUsers temporarySnapshot
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
mySymbolListSnapshot

	^SymbolDictionarySnapshotter forMe temporarySnapshot
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
snapshotFromSourceMatching: aString

	aString = 'Everything' ifTrue: [^self allUsersSnapshot].
	aString = 'My SymbolList' ifTrue: [^self mySymbolListSnapshot].
	^self symbolListSnapshotForUserProfile: (self userProfileCorrespondingToSourceDescription: aString)
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
symbolListSnapshotForUserProfile: aUserProfile

	^(SymbolDictionarySnapshotter forUserId: aUserProfile userId) temporarySnapshot
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
takeAllUsersSnapshot

	self commitSnapshot: self allUsersSnapshot
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
takeMySymbolListSnapshot

	self commitSnapshot: self mySymbolListSnapshot
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
takePrivateSnapshotForAllUsers

	self updatePrivateRepositoryUsing: [self takeAllUsersSnapshot]
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
takePublicSnapshotForAllUsers

	self updatePublicRepositoryUsing: [self takeAllUsersSnapshot]
%
category: 'Snapshotting'
set compile_env: 0
method: SymbolDictionarySnapshotManager
takeSnapshotFromSourceMatching: aString

	self commitSnapshot: (self snapshotFromSourceMatching: aString)
%
category: 'Snapshotting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
commitSnapshot: snapshot

	self repository at: (self repositoryKeyForSnapshot: snapshot) put: snapshot.
	System commit
%
category: 'Snapshotting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
updatePrivateRepositoryUsing: aBlock

	self updateRepositoryFrom: self privateRepositorySource using: aBlock
%
category: 'Snapshotting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
updatePublicRepositoryUsing: aBlock

	self updateRepositoryFrom: self publicRepositorySource using: aBlock
%
category: 'Snapshotting - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
updateRepositoryFrom: aSymbolDictionary using: aBlock

	| currentRepository |
	currentRepository := self repository.
	self useRepositoryFrom: aSymbolDictionary.
	aBlock value.
	self repository: currentRepository.

%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotManager
deleteSnapshot: snapshot
	"It is an error to delete a snapshot that doesn't exist."

	self deleteSnapshotOf: (self repositoryKeyForSnapshot: snapshot)
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotManager
deleteSnapshotDescribedAs: aString

	self deleteSnapshotOf: (self repositoryKeyFromDescription: aString).
	System commit
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotManager
deleteSnapshotOf: aDateAndTime
	"It is an error to delete a snapshot that doesn't exist."

	self repository removeKey: aDateAndTime
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotManager
usePrivateRepository

	self useRepositoryFrom: self privateRepositorySource
%
category: 'Updating'
set compile_env: 0
method: SymbolDictionarySnapshotManager
usePublicRepository

	self useRepositoryFrom: self publicRepositorySource
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
repository: aDictionary

	repository := aDictionary
%
category: 'Updating - private'
set compile_env: 0
method: SymbolDictionarySnapshotManager
useRepositoryFrom: aSymbolDictionary

	self repository: (aSymbolDictionary at: #'Source Code Snapshots' ifAbsentPut: [Dictionary new])
%






!
! From ! GEMSTONE: 3.2.0, Thu Jun 20 14:06:03 2013 rsargent private build; IMAGE: GemStone/S64 v3.2.0 kernel classes filein completed at 20/06/2013 14:15:17

! 

! On July 17, 2013, 4:46:54 PM
!
!
! SymbolDictionary 'UserGlobals'
!
run
| symList newDict |
symList := System myUserProfile symbolList.
symList do: [ :element |
    (element includesKey: #UserGlobals)
        ifTrue: [ ^element ]
].
newDict := SymbolDictionary new.
newDict at: #UserGlobals put: newDict.
System myUserProfile insertDictionary: newDict at: 1.
^newDict
%
doit
(Error subclass: 'CypressLoaderError'  instVarNames: #( patchOperation exception)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #( disallowGciStore)) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressLoaderError
doit
CypressLoaderError comment: 
'CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.
The CypressLoader made a first attempt to apply the Patch Operation and reported a 
CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applying
all other Patch Operations.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
'
%
doit
(Error subclass: 'CypressLoaderMissingClasses'  instVarNames: #( requirementsMap)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #( disallowGciStore)) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressLoaderMissingClasses
doit
CypressLoaderMissingClasses comment: 
''
%
doit
(Notification subclass: 'CypressLoaderErrorNotification'  instVarNames: #( patchOperation exception)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #( disallowGciStore)) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressLoaderErrorNotification
doit
CypressLoaderErrorNotification comment: 
'CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
'
%
doit
(Object subclass: 'CypressJsonParser'  instVarNames: #( stream)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%

! ------------------- Class comment for CypressJsonParser
doit
CypressJsonParser comment: 
''
%
doit
(Object subclass: 'CypressMockBasic'  instVarNames: #( name)  classVars: #( Something)  classInstVars: #( current)  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Mocks'
.
%

! ------------------- Class comment for CypressMockBasic
doit
CypressMockBasic comment: 
'This mock contains basic class and instance method selectors'
%
doit
(Object subclass: 'CypressObject'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressObject
doit
CypressObject comment: 
''
%
doit
(CypressObject subclass: 'CypressDefinition'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressDefinition
doit
CypressDefinition comment: 
''
%
doit
(CypressDefinition subclass: 'CypressClassDefinition'  instVarNames: #( name superclassName category                    comment instVarNames classInstVarNames classVarNames)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressClassDefinition
doit
CypressClassDefinition comment: 
''
%
doit
(CypressDefinition subclass: 'CypressMethodDefinition'  instVarNames: #( classIsMeta source category                    selector className)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressMethodDefinition
doit
CypressMethodDefinition comment: 
''
%
doit
(CypressObject subclass: 'CypressDefinitionIndex'  instVarNames: #( definitionMap)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressDefinitionIndex
doit
CypressDefinitionIndex comment: 
''
%
doit
(CypressObject subclass: 'CypressDependencySorter'  instVarNames: #( required provided orderedItems)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressDependencySorter
doit
CypressDependencySorter comment: 
''
%
doit
(CypressObject subclass: 'CypressLoader'  instVarNames: #( additions removals unloadable                    provisions errors methodAdditions requirements                    exceptionClass)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressLoader
doit
CypressLoader comment: 
''
%
doit
(CypressObject subclass: 'CypressPackageDefinition'  instVarNames: #( name)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressPackageDefinition
doit
CypressPackageDefinition comment: 
''
%
doit
(CypressObject subclass: 'CypressPatch'  instVarNames: #( operations)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressPatch
doit
CypressPatch comment: 
''
%
doit
(CypressObject subclass: 'CypressPatchOperation'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressPatchOperation
doit
CypressPatchOperation comment: 
''
%
doit
(CypressPatchOperation subclass: 'CypressAddition'  instVarNames: #( definition)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressAddition
doit
CypressAddition comment: 
''
%
doit
(CypressPatchOperation subclass: 'CypressModification'  instVarNames: #( obsoletion modification)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressModification
doit
CypressModification comment: 
''
%
doit
(CypressPatchOperation subclass: 'CypressRemoval'  instVarNames: #( definition)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressRemoval
doit
CypressRemoval comment: 
''
%
doit
(CypressObject subclass: 'CypressSnapshot'  instVarNames: #( definitions)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%

! ------------------- Class comment for CypressSnapshot
doit
CypressSnapshot comment: 
''
%
doit
(CypressObject subclass: 'CypressStructure'  instVarNames: #( name properties packageStructure)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%

! ------------------- Class comment for CypressStructure
doit
CypressStructure comment: 
''
%
doit
(CypressStructure subclass: 'CypressClassStructure'  instVarNames: #( instanceMethods classMethods comment                    isClassExtension)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%

! ------------------- Class comment for CypressClassStructure
doit
CypressClassStructure comment: 
''
%
doit
(CypressStructure subclass: 'CypressMethodStructure'  instVarNames: #( source isMetaclass classStructure)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%

! ------------------- Class comment for CypressMethodStructure
doit
CypressMethodStructure comment: 
''
%
doit
(CypressStructure subclass: 'CypressPackageStructure'  instVarNames: #( classes extensions)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Structure'
.
%

! ------------------- Class comment for CypressPackageStructure
doit
CypressPackageStructure comment: 
''
%
doit
(TestCase subclass: 'CypressAbstractTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressAbstractTest
doit
CypressAbstractTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressDefinitionTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressDefinitionTest
doit
CypressDefinitionTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressExtensionsTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #())
.
%

! ------------------- Class comment for CypressExtensionsTest
doit
CypressExtensionsTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressLoaderTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressLoaderTest
doit
CypressLoaderTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressPatchTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressPatchTest
doit
CypressPatchTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressSnapshotTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressSnapshotTest
doit
CypressSnapshotTest comment: 
''
%
doit
(CypressAbstractTest subclass: 'CypressStructureTest'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Tests'
.
%

! ------------------- Class comment for CypressStructureTest
doit
CypressStructureTest comment: 
''
%
doit
CypressLoaderError immediateInvariant.
%
doit
CypressLoaderMissingClasses immediateInvariant.
%
doit
CypressLoaderErrorNotification immediateInvariant.
%
doit
CypressJsonParser immediateInvariant.
%
doit
CypressMockBasic immediateInvariant.
%
doit
CypressObject immediateInvariant.
%
doit
CypressDefinition immediateInvariant.
%
doit
CypressClassDefinition immediateInvariant.
%
doit
CypressMethodDefinition immediateInvariant.
%
doit
CypressDefinitionIndex immediateInvariant.
%
doit
CypressDependencySorter immediateInvariant.
%
doit
CypressLoader immediateInvariant.
%
doit
CypressPackageDefinition immediateInvariant.
%
doit
CypressPatch immediateInvariant.
%
doit
CypressPatchOperation immediateInvariant.
%
doit
CypressAddition immediateInvariant.
%
doit
CypressModification immediateInvariant.
%
doit
CypressRemoval immediateInvariant.
%
doit
CypressSnapshot immediateInvariant.
%
doit
CypressStructure immediateInvariant.
%
doit
CypressClassStructure immediateInvariant.
%
doit
CypressMethodStructure immediateInvariant.
%
doit
CypressPackageStructure immediateInvariant.
%
doit
CypressAbstractTest immediateInvariant.
%
doit
CypressDefinitionTest immediateInvariant.
%
doit
CypressExtensionsTest immediateInvariant.
%
doit
CypressLoaderTest immediateInvariant.
%
doit
CypressPatchTest immediateInvariant.
%
doit
CypressSnapshotTest immediateInvariant.
%
doit
CypressStructureTest immediateInvariant.
%

! Remove existing behavior from CypressLoaderError
doit
CypressLoaderError removeAllMethods.
CypressLoaderError class removeAllMethods.
%
! ------------------- Class methods for CypressLoaderError
category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderError
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%
! ------------------- Instance methods for CypressLoaderError
category: 'accessing'
set compile_env: 0
method: CypressLoaderError
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%
category: 'accessing'
set compile_env: 0
method: CypressLoaderError
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%
category: 'handling'
set compile_env: 0
method: CypressLoaderError
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initialize

	super initialize.
	gsResumable := true
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderError
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%
category: 'updating'
set compile_env: 0
method: CypressLoaderError
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%
category: 'updating'
set compile_env: 0
method: CypressLoaderError
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Remove existing behavior from CypressLoaderMissingClasses
doit
CypressLoaderMissingClasses removeAllMethods.
CypressLoaderMissingClasses class removeAllMethods.
%
! ------------------- Class methods for CypressLoaderMissingClasses
category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderMissingClasses
missingRequirementsMap: aDictionary
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself
%
! ------------------- Instance methods for CypressLoaderMissingClasses
category: 'accessing'
set compile_env: 0
method: CypressLoaderMissingClasses
requirementsMap
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

   ^requirementsMap
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initialize

	super initialize.
	gsResumable := true
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream nextPutAll: 'Missing classes:'.
					self requirementsMap keysAndValuesDo: 
							[:className :definitions |
							stream
								space;
								nextPutAll: className printString , '(' , definitions size printString
											, ')']]
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderMissingClasses
initializeRequirementsMap: aDictionary

	self
		requirementsMap: aDictionary;
		initializeMessageText.
%
category: 'updating'
set compile_env: 0
method: CypressLoaderMissingClasses
requirementsMap: aDictionary
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

	requirementsMap := aDictionary
%

! Remove existing behavior from CypressLoaderErrorNotification
doit
CypressLoaderErrorNotification removeAllMethods.
CypressLoaderErrorNotification class removeAllMethods.
%
! ------------------- Class methods for CypressLoaderErrorNotification
category: 'instance creation'
set compile_env: 0
classmethod: CypressLoaderErrorNotification
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%
! ------------------- Instance methods for CypressLoaderErrorNotification
category: 'accessing'
set compile_env: 0
method: CypressLoaderErrorNotification
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%
category: 'accessing'
set compile_env: 0
method: CypressLoaderErrorNotification
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%
category: 'handling'
set compile_env: 0
method: CypressLoaderErrorNotification
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction
%
category: 'handling'
set compile_env: 0
method: CypressLoaderErrorNotification
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderErrorNotification
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]
%
category: 'initializing - private'
set compile_env: 0
method: CypressLoaderErrorNotification
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%
category: 'updating'
set compile_env: 0
method: CypressLoaderErrorNotification
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%
category: 'updating'
set compile_env: 0
method: CypressLoaderErrorNotification
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Remove existing behavior from CypressJsonParser
doit
CypressJsonParser removeAllMethods.
CypressJsonParser class removeAllMethods.
%
! ------------------- Class methods for CypressJsonParser
category: 'accessing'
set compile_env: 0
classmethod: CypressJsonParser
parse: aString
	^ self parseStream: aString readStream
%
category: 'accessing'
set compile_env: 0
classmethod: CypressJsonParser
parseStream: aStream
	^ (self on: aStream) parse
%
category: 'instance creation'
set compile_env: 0
classmethod: CypressJsonParser
new
	self error: 'Instantiate the parser with a stream.'
%
category: 'instance creation'
set compile_env: 0
classmethod: CypressJsonParser
on: aStream
	^ self basicNew initializeOn: aStream
%
! ------------------- Instance methods for CypressJsonParser
category: 'adding'
set compile_env: 0
method: CypressJsonParser
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
%
category: 'adding'
set compile_env: 0
method: CypressJsonParser
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString
%
category: 'creating'
set compile_env: 0
method: CypressJsonParser
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true
%
category: 'initialization'
set compile_env: 0
method: CypressJsonParser
initializeOn: aStream
	stream := aStream
%
category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parse
	| result |
	result := self whitespace; parseValue.
	stream atEnd
		ifFalse: [ self error: 'end of input expected' ].
	^ result
%
category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseArray
	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addValue: self parseValue
			to: result.
		(self match: ']') 
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of array expected'
%
category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseObject
	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addProperty: self parseProperty
			to: result.
		(self match: '}')
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of object expected'
%
category: 'parsing'
set compile_env: 0
method: CypressJsonParser
parseValue
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	self error: 'invalid input'
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacter
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
		ifTrue: [ ^ char ].
	char = $b
		ifTrue: [ ^ Character backspace ].
	char = $f
		ifTrue: [ ^ Character newPage ].
	char = $n
		ifTrue: [ ^ Character lf ].
	char = $r
		ifTrue: [ ^ Character cr ].
	char = $t
		ifTrue: [ ^ Character tab ].
	char = $u
		ifTrue: [ ^ self parseCharacterHex ].
	self error: 'invalid escape character \' , (String with: char)
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseCharacterHexDigit
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next codePoint.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    self error: 'hex-digit expected'
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumber
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	self whitespace.
	^ number
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next codePoint - 48).
            power := power * 10.0 ].
    ^ number / power
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    ^ number
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
%
category: 'parsing-internal'
set compile_env: 0
method: CypressJsonParser
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
%
category: 'private'
set compile_env: 0
method: CypressJsonParser
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^ (self match: aString) ifFalse: [ self error: aString , ' expected' ]
%
category: 'private'
set compile_env: 0
method: CypressJsonParser
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true
%
category: 'private'
set compile_env: 0
method: CypressJsonParser
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
%

! Remove existing behavior from CypressMockBasic
doit
CypressMockBasic removeAllMethods.
CypressMockBasic class removeAllMethods.
%
! ------------------- Class methods for CypressMockBasic
category: 'accessing'
set compile_env: 0
classmethod: CypressMockBasic
current
	^current
%
category: 'accessing'
set compile_env: 0
classmethod: CypressMockBasic
current: anObject
	current := anObject
%
category: 'initialization'
set compile_env: 0
classmethod: CypressMockBasic
initialize
	self current: self new
%
! ------------------- Instance methods for CypressMockBasic
category: 'accessing'
set compile_env: 0
method: CypressMockBasic
extra
	"extra method"
%
category: 'accessing'
set compile_env: 0
method: CypressMockBasic
name
	^name
%
category: 'accessing'
set compile_env: 0
method: CypressMockBasic
name: aString
	name := aString
%
category: 'initialization'
set compile_env: 0
method: CypressMockBasic
initialize
	super initialize.
	self name: 'Unknown'
%

! Remove existing behavior from CypressObject
doit
CypressObject removeAllMethods.
CypressObject class removeAllMethods.
%
! ------------------- Class methods for CypressObject
category: 'converting'
set compile_env: 0
classmethod: CypressObject
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	^aString withUnixLineEndings.
%
! ------------------- Instance methods for CypressObject
category: 'accessing'
set compile_env: 0
method: CypressObject
allClasses

	^System myUserProfile symbolList allSatisfying: [:each | each isBehavior]
%
category: 'accessing'
set compile_env: 0
method: CypressObject
classesInPackageNamed: aString

	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: [each category = aString or: [each category beginsWith: aString, '-']]])
		sortAscending: #('name')
%
category: 'accessing'
set compile_env: 0
method: CypressObject
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [self error: 'Could not resolve global named ' , aString printString]
%
category: 'accessing'
set compile_env: 0
method: CypressObject
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value
%
category: 'converting'
set compile_env: 0
method: CypressObject
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example."

	^self class normalizeLineEndings: aString.
%
category: 'initializing'
set compile_env: 0
method: CypressObject
initialize
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."
%
category: 'printing'
set compile_env: 0
method: CypressObject
printDetailsOn: aStream
%
category: 'printing'
set compile_env: 0
method: CypressObject
printOn: aStream

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.
%
category: 'sorting'
set compile_env: 0
method: CypressObject
addClasses: subs to: order fromRelevantClasses: classSet organizedBy: org

	1 to: subs size
		do: 
			[:i |
			| assoc class |
			class := subs at: i.
			(classSet includesIdentical: class) ifTrue: [order add: class].
			assoc := org associationAt: class otherwise: nil.
			assoc ~~ nil
				ifTrue: 
					[self
						addClasses: assoc value
						to: order
						fromRelevantClasses: classSet
						organizedBy: org]]
%
category: 'sorting'
set compile_env: 0
method: CypressObject
determineClassHierarchicalOrder: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."

	| org order classSet block |
	org := Dictionary new.
	org at: #nil put: ClassSet new.
	classSet := ClassSet new.
	someClasses do: 
			[:each |
			| sub |
			sub := each.
			sub isBehavior
				ifTrue: 
					[| superCls |
					classSet add: sub.
					
					[superCls := sub superClass.
					superCls ~~ nil] whileTrue: 
								[| assoc |
								assoc := org associationAt: superCls otherwise: nil.
								assoc
									ifNil: 
										[assoc := Association newWithKey: superCls value: ClassSet new.
										org add: assoc].
								assoc value add: sub.
								sub := superCls].
					(org at: #nil) add: sub]].

	"Order the subclass sets and weed out unwanted classes."
	order := Array new.
	self
		addClasses: (org at: #nil)
		to: order
		fromRelevantClasses: classSet
		organizedBy: org.
	^order
%

! Remove existing behavior from CypressDefinition
doit
CypressDefinition removeAllMethods.
CypressDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressDefinition
! ------------------- Instance methods for CypressDefinition
category: 'accessing'
set compile_env: 0
method: CypressDefinition
description
	self subclassResponsibility
%
category: 'comparing'
set compile_env: 0
method: CypressDefinition
= aDefinition

	^(aDefinition isKindOf: CypressDefinition)
		and: [aDefinition description = self description]
%
category: 'comparing'
set compile_env: 0
method: CypressDefinition
hash
    ^ self description hash
%
category: 'dependency'
set compile_env: 0
method: CypressDefinition
provisions
	"Answer list of global names defined by this definition"

	^#()
%
category: 'dependency'
set compile_env: 0
method: CypressDefinition
requirements
	"Answer list of global names required by this definition"

	^#()
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
actualClass

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
loadClassDefinition
	"default is to do nothing"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
loadMethodDefinition
	"default is to do nothing"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoad
	"noop"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoadOver: aDefinition

	self postLoad
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
unloadDefinition

	self subclassResponsibility
%
category: 'testing'
set compile_env: 0
method: CypressDefinition
isSameRevisionAs: aDefinition
	^ self = aDefinition
%
category: 'visiting'
set compile_env: 0
method: CypressDefinition
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
%

! Remove existing behavior from CypressClassDefinition
doit
CypressClassDefinition removeAllMethods.
CypressClassDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressClassDefinition
category: 'instance creation'
set compile_env: 0
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames comment: aComment

	^self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames: (someInstanceVariableNames asArray
				collect: [:each | each asString])
		classInstVarNames: (someClassInstanceVariableNames asArray
				collect: [:each | each asString])
		classVarNames: (someClassVariableNames asArray
				collect: [:each | each asString])
		comment: (self normalizeLineEndings: aComment)
%
! ------------------- Instance methods for CypressClassDefinition
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
category

	^category
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
classInstVarNames

	^classInstVarNames
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
className

	^self name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
classVarNames

	^classVarNames
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
comment

	^comment
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
description

	^ Array with: name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
instVarNames

	^instVarNames
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
name

	^name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
superclassName

	^superclassName
%
category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [classVarNames = aDefinition classVarNames
		and: [comment = aDefinition comment]]]]]]
%
category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
hash

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash
%
category: 'converting'
set compile_env: 0
method: CypressClassDefinition
asCypressClassDefinition

	^self
%
category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
provisions
	"Answer list of global names defined by this definition"

	^{ self name }
%
category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
requirements
	"Answer list of global names required by this definition"

	^{self superclassName}
%
category: 'initialization'
set compile_env: 0
method: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames comment: aComment

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	comment := aComment
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
actualClass

	^self resolveGlobalNamed: self name
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
actualClassOrNil

	^self resolveGlobalNamed: self name or: [nil]
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
classNeedingMigration: aClass

	self halt: 'not implemented yet'
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
createOrReviseClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		subclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: #()
		inDictionary: UserGlobals
		options: #())
			category: category;
			comment: self comment
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
failedCompiledMethods: someCompiledMethods

	someCompiledMethods isEmpty ifTrue: [^self].
	self halt: 'not implemented yet'
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
loadClassDefinition
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList

	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |
	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	removedClassVariables := olds difference: news.
	removedSharedPools := oldClass sharedPools difference: newClass sharedPools.
	self failedCompiledMethods: (newClass
				_copyMethodsAndVariablesFrom: oldClass
				except: { $V. removedClassVariables. $P. removedSharedPools }
				dictionaries: aSymbolList).
	organizer := ClassOrganizer new.
	subclasses := organizer subclassesOf: oldClass.


	"Do this -after- #subclassesOf:, which has the side effect of replacing the new
	  class with the old class in the organizer"
	organizer addClass: newClass.

	"Iterate over all the first-level subclasses of the old class to create new subclasses"
	subclasses do: 
			[:oldSubclass |
			newSubclass := 
					[oldSubclass definition evaluateInContext: nil symbolList: aSymbolList]
							on: Error
							do: [:ex | ex return: nil].
			(newSubclass notNil and: [newSubclass ~~ oldSubclass])
				ifTrue: 
					[self
						classNeedingMigration: newSubclass;
						recompileWithSubclassesFrom: oldSubclass
							to: newSubclass
							symbolList: aSymbolList]]
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
unloadDefinition
	"GemStone could hold multiple definitions of the same class name.
	 Ignore aliased references.
	 Unload only the first one resolved.
	 It is an error if there is not at least one SymbolDictionary holding a
	 class with that name."

	| dictionarySymbolPair |
	dictionarySymbolPair := ((System myUserProfile symbolList
				dictionariesAndSymbolsOf: self actualClass)
					select: [:each | each last = self name asSymbol]) first.
	dictionarySymbolPair first removeKey: dictionarySymbolPair last
%
category: 'printString'
set compile_env: 0
method: CypressClassDefinition
printDetailsOn: aStream

	aStream nextPutAll: self name
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
classInstanceVariablesString
    ^ self stringForVariables: self classInstVarNames
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
classVariablesString
    ^ self stringForVariables: self classVarNames
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
instanceVariablesString
    ^ self stringForVariables: self instVarNames
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
stringForVariables: variableList
    ^ String
        streamContents: [ :stream | variableList do: [ :ea | stream nextPutAll: ea ] separatedBy: [ stream space ] ]
%
category: 'visiting'
set compile_env: 0
method: CypressClassDefinition
classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
%

! Remove existing behavior from CypressMethodDefinition
doit
CypressMethodDefinition removeAllMethods.
CypressMethodDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressMethodDefinition
category: 'instance creation'
set compile_env: 0
classmethod: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndings: aSource)
%
! ------------------- Instance methods for CypressMethodDefinition
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
category

	^category
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
classIsMeta

	^classIsMeta
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
className

	^className
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
selector

	^selector
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
source

	^source
%
category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
%
category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
hash

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash
%
category: 'converting'
set compile_env: 0
method: CypressMethodDefinition
asCypressMethodDefinition

	^self
%
category: 'dependency'
set compile_env: 0
method: CypressMethodDefinition
requirements
	"Answer list of global names required by this definition"

	^{self className}
%
category: 'initialization'
set compile_env: 0
method: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndings: aSource
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
loadMethodDefinition

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
theNonMetaClass

	^self resolveGlobalNamed: self className
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
unloadDefinition

	self actualClass removeSelector: self selector asSymbol
%
category: 'printing'
set compile_env: 0
method: CypressMethodDefinition
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
%
category: 'testing'
set compile_env: 0
method: CypressMethodDefinition
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
%
category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
%
category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
%

! Remove existing behavior from CypressDefinitionIndex
doit
CypressDefinitionIndex removeAllMethods.
CypressDefinitionIndex class removeAllMethods.
%
! ------------------- Class methods for CypressDefinitionIndex
category: 'instance creation'
set compile_env: 0
classmethod: CypressDefinitionIndex
definitions: aCollection
	^ self new addAll: aCollection
%
! ------------------- Instance methods for CypressDefinitionIndex
category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
%
category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitions
	^self definitionMap values
%
category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
%
category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
addAll: aCollection
	aCollection do: [:ea | self add: ea]
%
category: 'querying'
set compile_env: 0
method: CypressDefinitionIndex
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
%
category: 'removing'
set compile_env: 0
method: CypressDefinitionIndex
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
%

! Remove existing behavior from CypressDependencySorter
doit
CypressDependencySorter removeAllMethods.
CypressDependencySorter class removeAllMethods.
%
! ------------------- Class methods for CypressDependencySorter
! ------------------- Instance methods for CypressDependencySorter
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
%
category: 'building'
set compile_env: 0
method: CypressDependencySorter
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
%
category: 'building'
set compile_env: 0
method: CypressDependencySorter
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
%

! Remove existing behavior from CypressLoader
doit
CypressLoader removeAllMethods.
CypressLoader class removeAllMethods.
%
! ------------------- Class methods for CypressLoader
category: 'loading'
set compile_env: 0
classmethod: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot
	"Answer the loader used to apply the update."

	^(self new)
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
%
! ------------------- Instance methods for CypressLoader
category: 'accessing'
set compile_env: 0
method: CypressLoader
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
errors
	errors ifNil: [self resetErrors].
	^errors
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
methodAdditions

	^#()
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
requirements

	^requirements
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyze

	self 
		analyzeAdditions;
		analyzeRemovals
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reverse.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
errorOnFailedPatchOperations

	exceptionClass := CypressLoaderError.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
handlePatchOperation: aPatchOperation failure: anException
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
load

	self
		analyze;
		reportUnloadableDefinitions;
		attemptInitialLoad;
		retryFailedLoads;
		postLoad.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
loadAdditions: somePatchOperations
	"Load class definitions first, then method definitions."

	somePatchOperations
		do: [:each | self loadClassDefinition: each];
		do: [:each | self loadMethodDefinition: each].
%
category: 'loading'
set compile_env: 0
method: CypressLoader
notifyOnFailedPatchOperations

	exceptionClass := CypressLoaderErrorNotification.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
%
category: 'loading'
set compile_env: 0
method: CypressLoader
reportUnloadableDefinitions

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
resetErrors

	errors := OrderedCollection new.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
retryFailedLoads
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self additions intersection: failed);
		unloadRemovals: (self removals intersection: failed).
%
category: 'loading'
set compile_env: 0
method: CypressLoader
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
%
category: 'loading'
set compile_env: 0
method: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
%
category: 'operations'
set compile_env: 0
method: CypressLoader
loadClassDefinition: aPatchOperation

	[aPatchOperation loadClassDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%
category: 'operations'
set compile_env: 0
method: CypressLoader
loadMethodDefinition: aPatchOperation

	[aPatchOperation loadMethodDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%
category: 'operations'
set compile_env: 0
method: CypressLoader
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%
category: 'operations'
set compile_env: 0
method: CypressLoader
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%
category: 'updating'
set compile_env: 0
method: CypressLoader
addFailedPatchOperation: aPatchOperation

	self errors add: aPatchOperation
%

! Remove existing behavior from CypressPackageDefinition
doit
CypressPackageDefinition removeAllMethods.
CypressPackageDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressPackageDefinition
! ------------------- Instance methods for CypressPackageDefinition
category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
classes

	^self classesInPackageNamed: self name
%
category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name
	^ name
%
category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name: aString
	name := aString
%
category: 'comparing'
set compile_env: 0
method: CypressPackageDefinition
= other
	^ other species = self species and: [other name sameAs: name]
%
category: 'printing'
set compile_env: 0
method: CypressPackageDefinition
printDetailsOn: aStream

	aStream nextPutAll: name
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addClass: aClass toDefinitions: definitions

	definitions add: aClass asCypressClassDefinition
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addExtensionMethodsFromClass: aClass toMap: classMap

	| defs map |
	defs := OrderedCollection new.
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			(category asLowercase beginsWith: '*' , self name asLowercase)
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category)
				do: [:selector | defs add: (aClass compiledMethodAt: selector) asCypressMethodDefinition]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	((aClass methodDictionary reject: [:each | each category first = $*])
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: method asCypressMethodDefinition]
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
snapshot

	| definitions map classMap |
	definitions := OrderedCollection new.
	(self determineClassHierarchicalOrder: self classes) do: 
			[:cls |
			self
				addClass: cls toDefinitions: definitions;
				addMethodsFromClass: cls toDefinitions: definitions;
				addMethodsFromClass: cls class toDefinitions: definitions].
	classMap := Dictionary new.
	self allClasses do: 
			[:each |
			self
				addExtensionMethodsFromClass: each toMap: classMap;
				addExtensionMethodsFromClass: each class toMap: classMap].
	(self determineClassHierarchicalOrder: classMap keys)
		do: [:aClass | definitions addAll: (classMap at: aClass)].
	^CypressSnapshot definitions: definitions
%

! Remove existing behavior from CypressPatch
doit
CypressPatch removeAllMethods.
CypressPatch class removeAllMethods.
%
! ------------------- Class methods for CypressPatch
category: 'instance creation'
set compile_env: 0
classmethod: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
%
! ------------------- Instance methods for CypressPatch
category: 'accessing'
set compile_env: 0
method: CypressPatch
operations

	^operations
%
category: 'applying'
set compile_env: 0
method: CypressPatch
applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
%
category: 'initialization'
set compile_env: 0
method: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]
%

! Remove existing behavior from CypressPatchOperation
doit
CypressPatchOperation removeAllMethods.
CypressPatchOperation class removeAllMethods.
%
! ------------------- Class methods for CypressPatchOperation
! ------------------- Instance methods for CypressPatchOperation
category: 'accessing'
set compile_env: 0
method: CypressPatchOperation
description

	self subclassResponsibility
%
category: 'applying'
set compile_env: 0
method: CypressPatchOperation
applyTo: aCypressLoader

	self subclassResponsibility
%
category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
= aPatchOperation
	^aPatchOperation isKindOf: self class
%
category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
hash
    ^ self description hash
%
category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility
%
category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadClassDefinition

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadMethodDefinition
	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
postLoadDefinition
	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
unloadDefinition

	self error: 'inappropriate to send #unloadDefinition to an addition or modification operation'
%
category: 'printing'
set compile_env: 0
method: CypressPatchOperation
printDetailsOn: aStream

	aStream nextPutAll: self description.
%

! Remove existing behavior from CypressAddition
doit
CypressAddition removeAllMethods.
CypressAddition class removeAllMethods.
%
! ------------------- Class methods for CypressAddition
category: 'instance creation'
set compile_env: 0
classmethod: CypressAddition
of: aDefinition
	^ self new definition: aDefinition
%
! ------------------- Instance methods for CypressAddition
category: 'accessing'
set compile_env: 0
method: CypressAddition
definition

	^definition
%
category: 'accessing'
set compile_env: 0
method: CypressAddition
description
    ^ 'add: ' , self definition printString
%
category: 'applying'
set compile_env: 0
method: CypressAddition
applyTo: aCypressLoader

	aCypressLoader applyAddition: self
%
category: 'comparing'
set compile_env: 0
method: CypressAddition
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%
category: 'dependency'
set compile_env: 0
method: CypressAddition
provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
%
category: 'dependency'
set compile_env: 0
method: CypressAddition
requirements
	"Answer list of global names required by this definition"

	^self definition requirements
%
category: 'initialization'
set compile_env: 0
method: CypressAddition
definition: aDefinition

	definition := aDefinition
%
category: 'loading'
set compile_env: 0
method: CypressAddition
loadClassDefinition

	self definition loadClassDefinition.
%
category: 'loading'
set compile_env: 0
method: CypressAddition
loadMethodDefinition

	self definition loadMethodDefinition.
%
category: 'loading'
set compile_env: 0
method: CypressAddition
postLoadDefinition
	self definition postLoadOver: nil
%

! Remove existing behavior from CypressModification
doit
CypressModification removeAllMethods.
CypressModification class removeAllMethods.
%
! ------------------- Class methods for CypressModification
category: 'instance creation'
set compile_env: 0
classmethod: CypressModification
of: base to: target
	^ self new base: base target: target
%
! ------------------- Instance methods for CypressModification
category: 'accessing'
set compile_env: 0
method: CypressModification
description
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString
%
category: 'accessing'
set compile_env: 0
method: CypressModification
modification

	^modification
%
category: 'accessing'
set compile_env: 0
method: CypressModification
obsoletion

	^obsoletion
%
category: 'applying'
set compile_env: 0
method: CypressModification
applyTo: aCypressLoader

	aCypressLoader applyModification: self
%
category: 'dependency'
set compile_env: 0
method: CypressModification
provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
%
category: 'dependency'
set compile_env: 0
method: CypressModification
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
%
category: 'initialization'
set compile_env: 0
method: CypressModification
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
%
category: 'initialization'
set compile_env: 0
method: CypressModification
base: base target: target

	obsoletion := base.
	modification := target.
%
category: 'loading'
set compile_env: 0
method: CypressModification
loadClassDefinition

	self modification loadClassDefinition.
%
category: 'loading'
set compile_env: 0
method: CypressModification
loadMethodDefinition

	self modification loadMethodDefinition.
%
category: 'loading'
set compile_env: 0
method: CypressModification
postLoadDefinition
	self modification postLoadOver: self obsoletion
%

! Remove existing behavior from CypressRemoval
doit
CypressRemoval removeAllMethods.
CypressRemoval class removeAllMethods.
%
! ------------------- Class methods for CypressRemoval
category: 'instance creation'
set compile_env: 0
classmethod: CypressRemoval
of: aDefinition
	^ self new definition: aDefinition
%
! ------------------- Instance methods for CypressRemoval
category: 'accessing'
set compile_env: 0
method: CypressRemoval
definition

	^definition
%
category: 'accessing'
set compile_env: 0
method: CypressRemoval
description

	^'remove: ', self definition printString
%
category: 'applying'
set compile_env: 0
method: CypressRemoval
applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
%
category: 'comparing'
set compile_env: 0
method: CypressRemoval
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%
category: 'dependency'
set compile_env: 0
method: CypressRemoval
provisions
	"Answer list of global names defined by this definition"

	^#()
%
category: 'dependency'
set compile_env: 0
method: CypressRemoval
requirements
	"Answer list of global names required by this definition"

	^#()
%
category: 'initialization'
set compile_env: 0
method: CypressRemoval
definition: aDefinition

	definition := aDefinition
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
loadClassDefinition
	
	self error: 'inappropriate to send #loadClassDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
loadMethodDefinition
	
	self error: 'inappropriate to send #loadMethodDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
postLoadDefinition
	
	self error: 'inappropriate to send #postLoadDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
unloadDefinition

	self definition unloadDefinition.
%

! Remove existing behavior from CypressSnapshot
doit
CypressSnapshot removeAllMethods.
CypressSnapshot class removeAllMethods.
%
! ------------------- Class methods for CypressSnapshot
category: 'instance creation'
set compile_env: 0
classmethod: CypressSnapshot
definitions: aDefinitions

	^(self new) definitions: aDefinitions
%
! ------------------- Instance methods for CypressSnapshot
category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions

	^definitions
%
category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions: aDefinitions

	definitions := aDefinitions
%
category: 'comparing'
set compile_env: 0
method: CypressSnapshot
= other
	^ definitions asArray = other definitions asArray
%
category: 'enumerating'
set compile_env: 0
method: CypressSnapshot
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
%
category: 'loading'
set compile_env: 0
method: CypressSnapshot
updatePackage: aPackage
	"Answer the loader used to apply the update."

	^CypressLoader updatePackage: aPackage withSnapshot: self
%
category: 'patching'
set compile_env: 0
method: CypressSnapshot
patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
%

! Remove existing behavior from CypressStructure
doit
CypressStructure removeAllMethods.
CypressStructure class removeAllMethods.
%
! ------------------- Class methods for CypressStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressStructure
fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
%
! ------------------- Instance methods for CypressStructure
category: 'accessing'
set compile_env: 0
method: CypressStructure
name

	^name
%
category: 'accessing'
set compile_env: 0
method: CypressStructure
name: aString 

	name := aString
%
category: 'accessing'
set compile_env: 0
method: CypressStructure
packageStructure
	^packageStructure
%
category: 'accessing'
set compile_env: 0
method: CypressStructure
packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
%
category: 'accessing'
set compile_env: 0
method: CypressStructure
properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
%
category: 'accessing'
set compile_env: 0
method: CypressStructure
properties: aDictionary

	properties := aDictionary
%
category: 'initialization'
set compile_env: 0
method: CypressStructure
fromJs: jsObject

	self subclassResponsibility
%
category: 'printing'
set compile_env: 0
method: CypressStructure
printDetailsOn: aStream

	aStream nextPutAll: self name.
%
category: 'writing'
set compile_env: 0
method: CypressStructure
path: aFSPath file: aFilename write: writeBlock

	| fs stream |
	fs := aFSPath fs.
	stream := fs createWriteStream: (aFSPath resolve: aFilename).
	writeBlock value: stream.
	stream end.
%
category: 'writing'
set compile_env: 0
method: CypressStructure
writeJsonOn: aStream

	self writeJsonOn: aStream indent: 0.
%
category: 'writing'
set compile_env: 0
method: CypressStructure
writeJsonOn: aStream  indent: indent

	self subclassResponsibility
%

! Remove existing behavior from CypressClassStructure
doit
CypressClassStructure removeAllMethods.
CypressClassStructure class removeAllMethods.
%
! ------------------- Class methods for CypressClassStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressClassStructure
fromClassDefinition: classDefinition

	^self new
		fromClassDefinition: classDefinition;
		yourself
%
! ------------------- Instance methods for CypressClassStructure
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
category

	^self packageStructure packageName
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classInstanceVariableNames

	^self properties at: 'classinstvars' ifAbsent: [#()]
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classInstanceVariableNames: someStrings

	^self properties at: 'classinstvars' put: someStrings
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
className

	^self name
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classVariableNames

	^self properties at: 'classvars' ifAbsent: [#()]
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
classVariableNames: someStrings

	^self properties at: 'classvars' put: someStrings
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
comment

	comment ifNil: [ comment := '' ].
	^comment
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
comment: aString

	comment := aString
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceVariableNames

	^self properties at: 'instvars' ifAbsent: [#()]
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
instanceVariableNames: someStrings

	^self properties at: 'instvars' put: someStrings
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
isClassExtension: aBoolean

	isClassExtension := aBoolean
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
name

	^self properties at: 'name'
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
name: aString

	self properties at: 'name' put: aString
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
superclassName

	^self properties at: 'super'
%
category: 'accessing'
set compile_env: 0
method: CypressClassStructure
superclassName: aString

	^self properties at: 'super' put: aString
%
category: 'converting'
set compile_env: 0
method: CypressClassStructure
asCypressClassDefinition

	self isClassExtension ifTrue: [^nil].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		classVarNames: self classVariableNames
		comment: self comment
%
category: 'initialization'
set compile_env: 0
method: CypressClassStructure
fromClassDefinition: classDefinition

	self
		isClassExtension: false;
		name: classDefinition name;
		comment: classDefinition comment;
		superclassName: classDefinition superclassName;
		instanceVariableNames: classDefinition instVarNames;
		classInstanceVariableNames: classDefinition classInstVarNames;
		classVariableNames: classDefinition classVarNames.
%
category: 'initialization'
set compile_env: 0
method: CypressClassStructure
fromJs: jsObject

	properties := jsObject at: 'properties.json'.
	(jsObject at: 'class' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self classMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			isMetaclass: true;
			fromJs: jsMethodObject named: methodNameParts ].
	(jsObject at: 'instance' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self instanceMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			fromJs: jsMethodObject named: methodNameParts ].	
	comment := jsObject at: 'README.md' ifAbsent: ['']
%
category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodName: methodName

	| extension |
	extension := #('.json' '.st')
		detect: [:each | methodName endsWith: each] 
		ifNone: [self error: 'invalid structure element: ', methodName].
	^Array
		with: (methodName copyWithoutSuffix: extension)
		with: extension.
%
category: 'private'
set compile_env: 0
method: CypressClassStructure
splitMethodNameFor: jsMethodObject

	^self splitMethodName: (jsMethodObject at: 'name')
%
category: 'querying'
set compile_env: 0
method: CypressClassStructure
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure new name: methodName]
%
category: 'querying'
set compile_env: 0
method: CypressClassStructure
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure new name: methodName]
%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJson: aString methods: someMethodStructures on: aStream  indent: indent

	| methods |
	aStream
		tab: indent;
		nextPutAll: '"', aString, '" : [';
		lf;
		yourself.
	(someMethodStructures asSortedCollection: [:a :b | a selector <= b selector])
	doWithIndex: [:methodStructure :index |
		index > 1 ifTrue: [ aStream nextPutAll: ','; lf ].
		methodStructure writeJsonOn: aStream indent: indent + 1].
	aStream
		tab: indent;
		nextPutAll: '],';
		lf;
		yourself.
%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonCommentOn: aStream  indent: indent

	self isClassExtension ifTrue: [^self].
	aStream
		tab: indent;
		nextPutAll: '"README.md" : ';
		yourself.
	self comment writeCypressJsonOn: aStream indent: indent.
	aStream
		nextPutAll: ',';
		lf;
		yourself.

%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonNameOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		lf.
%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonOn: aStream indent: startIndent

	| indent |
	aStream
		tab: startIndent;
		nextPutAll: '{';
		lf.
	indent := startIndent + 1.
	self
		writeJsonNameOn: aStream indent: indent;
		writeJson: 'instance' methods: self instanceMethods on: aStream indent: indent;
		writeJson: 'class' methods: self classMethods on: aStream indent: indent;
		writeJsonCommentOn: aStream indent: indent;
		writeJsonPropertiesOn: aStream indent: indent.
	aStream
		lf;
		tab: startIndent;
		nextPutAll: ' }'
%
category: 'writing'
set compile_env: 0
method: CypressClassStructure
writeJsonPropertiesOn: aStream  indent: indent

	aStream
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
%

! Remove existing behavior from CypressMethodStructure
doit
CypressMethodStructure removeAllMethods.
CypressMethodStructure class removeAllMethods.
%
! ------------------- Class methods for CypressMethodStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressMethodStructure
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
%
! ------------------- Instance methods for CypressMethodStructure
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
category

	^self properties at: 'category'
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
category: aString

	self properties at: 'category' put: aString
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
classStructure
	^classStructure
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
cypressSource

	| stream |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: self category;
		lf;
		nextPutAll: self source.
	^stream contents
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
isMetaclass: aBoolean
	isMetaclass := aBoolean
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
selector
    ^ String
        streamContents: [ :stream | 
            self name
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $.
                                ifTrue: [ $: ]
                                ifFalse: [ chara ]) ] ]
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
selector: aString
    name := String
        streamContents: [ :stream | 
            aString
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $:
                                ifTrue: [ $. ]
                                ifFalse: [ chara ]) ] ]
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
source

	^source
%
category: 'accessing'
set compile_env: 0
method: CypressMethodStructure
source: aString

	source := aString
%
category: 'converting'
set compile_env: 0
method: CypressMethodStructure
asCypressMethodDefinition

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source
%
category: 'initialization'
set compile_env: 0
method: CypressMethodStructure
fromJs: jsObject named: methodNameParts
    | ext |
    (ext := methodNameParts at: 2) = '.st'
        ifTrue: [ self extractCypressSource: (jsObject at: 'contents') ]
        ifFalse: [ 
            ext = '.json'
                ifTrue: [ properties := jsObject at: 'contents' ] ]
%
category: 'initialization'
set compile_env: 0
method: CypressMethodStructure
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
%
category: 'private'
set compile_env: 0
method: CypressMethodStructure
extractCypressSource: aString
    | stream categoryStream sourceStream readingCategory |
    stream := ReadStream on: aString.
    categoryStream := WriteStream on: String new.
    sourceStream := WriteStream on: String new.
    readingCategory := true.
    [ stream atEnd ]
        whileFalse: [ 
            | char |
            char := stream next.
            readingCategory
                ifTrue: [ 
                    char = Character lf
                        ifTrue: [ readingCategory := false ]
                        ifFalse: [ categoryStream nextPut: char ] ]
                ifFalse: [ sourceStream nextPut: char ] ].
    self category: categoryStream contents.
    self source: sourceStream contents
%
category: 'writing'
set compile_env: 0
method: CypressMethodStructure
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '.st",';
		lf.
	aStream
		tab: indent;
		nextPutAll: '"contents"';
		nextPutAll: ' : '.
	self cypressSource writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		lf;
		tab: indent;
		nextPutAll: ' }'
%

! Remove existing behavior from CypressPackageStructure
doit
CypressPackageStructure removeAllMethods.
CypressPackageStructure class removeAllMethods.
%
! ------------------- Class methods for CypressPackageStructure
category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fromJson: aJsonString

	^self fromJs: (CypressJsonParser parse: aJsonString)
%
category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageStructure
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
%
! ------------------- Instance methods for CypressPackageStructure
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
%
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
%
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageExtension

	^self properties at: 'extension' ifAbsent: ['.package' ]
%
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageName

	^self name copyFrom: 1 to: (self name size - self packageExtension size)
%
category: 'accessing'
set compile_env: 0
method: CypressPackageStructure
packageStructure
	^self
%
category: 'conversion'
set compile_env: 0
method: CypressPackageStructure
asCypressJson

	| stream |
    	stream := WriteStream on: String new.
    	self writeJsonOn: stream.
	^stream contents
%
category: 'initialization'
set compile_env: 0
method: CypressPackageStructure
fromJs: jsObject

	name := jsObject at: 'name'.
	(jsObject at: 'contents') do: [:jsClassObject| | classStructure objectName |
		classStructure := (CypressClassStructure new)
                		packageStructure: self;
				yourself.
                ((objectName := jsClassObject at: 'name') endsWith: '.extension')
			ifTrue: [ 
				classStructure isClassExtension: true.
				self extensions add: classStructure ]
			ifFalse: [
				((objectName := jsClassObject at: 'name') endsWith: '.class')
					ifTrue: [ 
						classStructure isClassExtension: false.
						self classes add: classStructure ]].
		classStructure fromJs: jsClassObject].
	properties := jsObject at: 'properties.json'
%
category: 'initialization'
set compile_env: 0
method: CypressPackageStructure
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, '.package'.
	properties := Dictionary new.
	classDefinitions := Set new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsent: [classMap at: methodDefinition className put: Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure new name: className)
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageStructure
snapshot
	| definitions map  |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions
%
category: 'writing'
set compile_env: 0
method: CypressPackageStructure
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '",'.
	aStream
		lf;
		tab: indent;
		nextPutAll: '"contents" : [';
		lf;
		yourself.
	1 to: self classes size do: [:index | | classStructure | 
		classStructure := self classes at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		(self extensions size > 0 or: [ index < self classes size]) ifTrue: [ aStream nextPutAll: ','; lf. ]].
	1 to: self extensions size do: [:index | | classStructure | 
		classStructure := self extensions at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		index < self extensions size ifTrue: [ aStream nextPutAll: ','; lf.] ].
	aStream
		lf;
		tab: indent;
		nextPutAll: '],';
		lf;
		tab: indent;
		nextPutAll: '"properties.json" : '.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream 
		lf;
		tab: indent;
		nextPutAll: '}'
%

! Remove existing behavior from CypressAbstractTest
doit
CypressAbstractTest removeAllMethods.
CypressAbstractTest class removeAllMethods.
%
! ------------------- Class methods for CypressAbstractTest
category: 'testing'
set compile_env: 0
classmethod: CypressAbstractTest
isAbstract

	^self sunitName = #CypressAbstractTest
%
! ------------------- Instance methods for CypressAbstractTest
category: 'private'
set compile_env: 0
method: CypressAbstractTest
baseDefinitions

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #('current')
			classVarNames: #('Something')
			comment: 'This mock contains basic class and instance method selectors'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'extra'
			category: 'accessing'
			source: 'extra
	"extra method"
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	super initialize.
	self name: ''Unknown''
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name'
			category: 'accessing'
			source: 'name
	^name
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name:'
			category: 'accessing'
			source: 'name: aString
	name := aString
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current'
			category: 'accessing'
			source: 'current
	^current
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current:'
			category: 'accessing'
			source: 'current: anObject
	current := anObject
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	self current: self new
'.
		CypressMethodDefinition
			className: 'Object'
			classIsMeta: false
			selector: 'isCypressMockBasic'
			category: '*Cypress-Mocks-Extensions'
		source: 'isCypressMockBasic

	^false'.
	}
%
category: 'private'
set compile_env: 0
method: CypressAbstractTest
basePackageStructureJson
    ^ '{
	"name" : "Cypress-Mocks.package",
	"contents" : [
		{
			"name" : "CypressMockBasic.class",
			"instance" : [
				{
					"name" : "extra.st",
					"contents" : "accessing%0Aextra%0A%09%22extra%20method%22%0A"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09super%20initialize.%0A%09self%20name%3A%20%27Unknown%27%0A"
				 },
				{
					"name" : "name.st",
					"contents" : "accessing%0Aname%0A%09%5Ename%0A"
				 },
				{
					"name" : "name..st",
					"contents" : "accessing%0Aname%3A%20aString%0A%09name%20%3A%3D%20aString%0A"
				 }			],
			"class" : [
				{
					"name" : "current.st",
					"contents" : "accessing%0Acurrent%0A%09%5Ecurrent%0A"
				 },
				{
					"name" : "current..st",
					"contents" : "accessing%0Acurrent%3A%20anObject%0A%09current%20%3A%3D%20anObject%0A"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09self%20current%3A%20self%20new%0A"
				 }			],
			"README.md" : "This%20mock%20contains%20basic%20class%20and%20instance%20method%20selectors",
			"properties.json" : {
				"classinstvars" : [
					"current" ],
				"classvars" : [
					"Something" ],
				"instvars" : [
					"name" ],
				"name" : "CypressMockBasic",
				"super" : "Object" }
		 },
		{
			"name" : "Object.extension",
			"instance" : [
				{
					"name" : "isCypressMockBasic.st",
					"contents" : "%2ACypress-Mocks-Extensions%0AisCypressMockBasic%0A%0A%09%5Efalse"
				 }			],
			"class" : [
			],
			"properties.json" : {
				"name" : "Object" }
		 }
	],
	"properties.json" : {
		 }
}'
%
category: 'private'
set compile_env: 0
method: CypressAbstractTest
baseTargetPatch

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressModification
			of: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks'
				instVarNames: #('name')
				classInstVarNames: #('current')
				classVarNames: #('Something')
				comment: 'This mock contains basic class and instance method selectors')
			to: (CypressClassDefinition
				name: className
				superclassName: 'Object'
				category: 'Cypress-Mocks'
				instVarNames: #('name')
				classInstVarNames: #('current')
				classVarNames: #()
				comment: 'This mock contains basic class and instance method selectors').
		CypressAddition
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'added'
				category: 'accessing'
				source: 'added
	"added method"
').
		CypressModification
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'name:'
				category: 'accessing'
				source: 'name: aString
	name := aString
')
			to: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'name:'
				category: 'accessing'
				source: 'name: aString
	"changed method"
	name := aString
').
		CypressRemoval
			of: (CypressMethodDefinition
				className: className
				classIsMeta: false
				selector: 'extra'
				category: 'accessing'
				source: 'extra
	"extra method"
').
		CypressRemoval
			of: (CypressMethodDefinition
				className: 'Object'
				classIsMeta: false
				selector: 'isCypressMockBasic'
				category: '*Cypress-Mocks-Extensions'
				source: 'isCypressMockBasic

	^false').
		CypressAddition
			of: (CypressClassDefinition
				name: className , 'Sub'
				superclassName: className
				category: 'Cypress-Mocks'
				instVarNames: #('anotherIV')
				classInstVarNames: #('anotherCIV')
				classVarNames: #()
				comment: 'Hacked subclass to test class loading and unloading').
		CypressAddition
			of: (CypressMethodDefinition
				className: className , 'Sub'
				classIsMeta: false
				selector: 'added'
				category: 'accessing'
				source: 'added
	"added method"
').
	}
%
category: 'private'
set compile_env: 0
method: CypressAbstractTest
sampleJson
    ^ '{
	"age" : 25,
	"name" : "John%20Smith",
	"phoneNumber" : [
		{
			"number" : "212%20555-1234",
			"type" : "home" },
		{
			"number" : "646%20555-4567",
			"type" : "fax" } ],
	"registered" : true }'
%
category: 'private'
set compile_env: 0
method: CypressAbstractTest
targetDefinitions
	"remove #extra method and modify #name: method"

	| className |
	className := 'CypressMockBasic'.
	^{
		CypressClassDefinition
			name: className
			superclassName: 'Object'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #('current')
			classVarNames: #()
			comment: 'This mock contains basic class and instance method selectors'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'added'
			category: 'accessing'
			source: 'added
	"added method"
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	super initialize.
	self name: ''Unknown''
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name'
			category: 'accessing'
			source: 'name
	^name
'.
		CypressMethodDefinition
			className: className
			classIsMeta: false
			selector: 'name:'
			category: 'accessing'
			source: 'name: aString
	"changed method"
	name := aString
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current'
			category: 'accessing'
			source: 'current
	^current
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'current:'
			category: 'accessing'
			source: 'current: anObject
	current := anObject
'.
		CypressMethodDefinition
			className: className
			classIsMeta: true
			selector: 'initialize'
			category: 'initialization'
			source: 'initialize
	self current: self new
'.
		CypressClassDefinition
			name: className , 'Sub'
			superclassName: className
			category: 'Cypress-Mocks'
			instVarNames: #('anotherIV')
			classInstVarNames: #('anotherCIV')
			classVarNames: #()
			comment: 'Hacked subclass to test class loading and unloading'.
		CypressMethodDefinition
			className: className , 'Sub'
			classIsMeta: false
			selector: 'added'
			category: 'accessing'
			source: 'added
	"added method"
'.
	}
%

! Remove existing behavior from CypressDefinitionTest
doit
CypressDefinitionTest removeAllMethods.
CypressDefinitionTest class removeAllMethods.
%
! ------------------- Class methods for CypressDefinitionTest
! ------------------- Instance methods for CypressDefinitionTest
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testClassDefinition

	self
		assert: (CypressClassDefinition
					name: 'Foo'
					superclassName: 'Object'
					category: 'Foo'
					instVarNames: #()
					classInstVarNames: #()
					classVarNames: #()
					comment: '') printString
		equals: 'a CypressClassDefinition(Foo)'
%
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testDictionaryOfDefinitions

	| dict |
	"baseDefinitions"
	dict := Dictionary new.
	self baseDefinitions do: [:each | 
		dict at: each put: each ].
	self baseDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].

	"targetDefinitions"
	dict := Dictionary new.
	self targetDefinitions do: [:each | 
		dict at: each put: each ].
	self targetDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].
%
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition new name: name.
	pkg2 := CypressPackageDefinition new name: name.
	pkg3 := CypressPackageDefinition new name: 'Nope!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 equals: pkg3
%
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString equals: 'a CypressMethodDefinition(Foo>>isFoo)'
%
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: pkg name equals: name.
	self deny: pkg name equals: 'Nope.'.
%
category: 'tests'
set compile_env: 0
method: CypressDefinitionTest
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
%

! Remove existing behavior from CypressExtensionsTest
doit
CypressExtensionsTest removeAllMethods.
CypressExtensionsTest class removeAllMethods.
%
! ------------------- Class methods for CypressExtensionsTest
! ------------------- Instance methods for CypressExtensionsTest
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_beginsWith_

	self
		assert: ('abc' beginsWith: 'a');
		deny: ('abc' beginsWith: 'c');
		assert: ('abc' beginsWith: 'abc');
		deny: ('abc' beginsWith: 'abcxxx');
		deny: ('abc' beginsWith: '');
		deny: ('' beginsWith: 'abc');
		deny: ('' beginsWith: '')
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_copyWithoutSuffix_

	self
		assert: ('abc' copyWithoutSuffix: 'c') equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: 'abc') equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: '') equals: 'abc';
		assert: ('' copyWithoutSuffix: 'abc') equals: '';
		assert: ('' copyWithoutSuffix: '') equals: ''
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_copyWithoutSuffix_or_

	self
		assert: ('abc' copyWithoutSuffix: 'c' or: [nil]) equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: 'abc' or: [nil]) equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: '' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: 'abc' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: '' or: [nil]) equals: nil
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_difference_
	"Set theoretic difference means elements from the first collection not in the second."

	self
		assert: (#(1 2 3) difference: #()) sortAscending equals: #(1 2 3);
		assert: (#() difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) difference: #(2 3 4)) sortAscending equals: #(1);
		assert: (#(1 2 3) difference: #(3 4 5)) sortAscending equals: #(1 2);
		assert: (#(1 2 3) difference: #(4 5 6)) sortAscending equals: #(1 2 3)
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_endsWith_

	self
		assert: ('abc' endsWith: 'c');
		deny: ('abc' endsWith: 'a');
		assert: ('abc' endsWith: 'abc');
		deny: ('abc' endsWith: 'xxxabc');
		deny: ('abc' endsWith: '');
		deny: ('' endsWith: 'abc');
		deny: ('' endsWith: '')
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_escapePercents

	self
		assert: 'aa aa éé aa aa' encodeAsUTF8 escapePercents
			equals: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa';
		assert: 'aa aa éé aa aa' escapePercents
			equals: 'aa%20aa%20%E9%E9%20aa%20aa'
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_findString_startingAt_caseSensitive_

	| string |
	string := 'abcABCdef'.
	self
		assert: (string findString: 'abc' startingAt: 1 caseSensitive: true) equals: 1;
		assert: (string findString: 'abc' startingAt: 1 caseSensitive: false) equals: 1;
		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: true) equals: 4;
		assert: (string findString: 'ABC' startingAt: 1 caseSensitive: false) equals: 1;
		assert: (string findString: 'def' startingAt: 1 caseSensitive: true) equals: 7;
		assert: (string findString: 'def' startingAt: 1 caseSensitive: false) equals: 7;
		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: true) equals: 0;
		assert: (string findString: 'DEF' startingAt: 1 caseSensitive: false) equals: 7
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_gather_

	self
		assert: (#(1 2 3) gather: [:each | each * 10 + 1 to: each * 10 + each])
		equals: #(11 21 22 31 32 33)
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_indexOfAnyOf_startingAt_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21) equals: 0;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1) equals: 0
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_indexOfAnyOf_startingAt_ifAbsent_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1 ifAbsent: [nil]) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6 ifAbsent: [nil]) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7 ifAbsent: [nil]) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15 ifAbsent: [nil]) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16 ifAbsent: [nil]) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21 ifAbsent: [nil]) equals: nil;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1 ifAbsent: [nil]) equals: nil
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_intersection_

	self
		assert: (#(1 2 3) intersection: #()) sortAscending equals: #();
		assert: (#() intersection: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) intersection: #(1 2 3)) sortAscending equals: #(1 2 3);
		assert: (#(1 2 3) intersection: #(2 3 4)) sortAscending equals: #(2 3);
		assert: (#(1 2 3) intersection: #(3 4 5)) sortAscending equals: #(3);
		assert: (#(1 2 3) intersection: #(4 5 6)) sortAscending equals: #()
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_streamContents_

	self
		assert: (String streamContents: 
					[:stream |
					(1 to: 3) do: [:each | stream nextPutAll: each printString]
						separatedBy: [stream space]])
		equals: '1 2 3'
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_unescapePercents

	self
		assert: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'  unescapePercents asByteArray decodeFromUTF8 asString
			equals: 'aa aa éé aa aa';
		assert: 'aa%20aa%20%E9%E9%20aa%20aa' unescapePercents
			equals: 'aa aa éé aa aa' asUnicodeString
%
category: 'tests'
set compile_env: 0
method: CypressExtensionsTest
test_withUnixLineEndings

	| lf cr crlf |
	lf := String with: Character lf.
	cr := String with: Character cr.
	crlf := cr, lf.

	self
		assert: (self unixLinesFrom: '') equals: #();
		assert: (self unixLinesFrom: 'abc') equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', cr) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', crlf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, crlf) equals: #('abc' '')
%
category: 'utility'
set compile_env: 0
method: CypressExtensionsTest
unixLinesFrom: aString
	
	| sourceStream resultStream |
	sourceStream := ReadStream on: aString withUnixLineEndings.
	resultStream := WriteStream on: Array new.
	[sourceStream atEnd]
		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].
	^resultStream contents.
%

! Remove existing behavior from CypressLoaderTest
doit
CypressLoaderTest removeAllMethods.
CypressLoaderTest class removeAllMethods.
%
! ------------------- Class methods for CypressLoaderTest
! ------------------- Instance methods for CypressLoaderTest
category: 'running'
set compile_env: 0
method: CypressLoaderTest
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition new name: name)
%
category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoad

	| name loader |
	name := 'Cypress-Mocks'.
	loader := (CypressSnapshot definitions: self targetDefinitions)
				updatePackage: (CypressPackageDefinition new name: name).
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements isEmpty
			description: 'There should have been no external requirements'
%
category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoaderWithClassDefinitionError

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition new name: name)]
		raise: CypressLoaderErrorNotification
		description: 'There should have been a class definition with an error'.
	loader := 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition new name: name)]
					on: CypressLoaderError
					do: 
						[:ex |
						self
							assert: ex patchOperation definition name
							equals: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'.
						ex resume].
	self
		assert: loader additions size equals: 4;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: (loader unloadable gather: [:each | each]) size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 1;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 0
%
category: 'tests'
set compile_env: 0
method: CypressLoaderTest
testLoaderWithUnloadable

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition new name: name)]
		raise: CypressLoaderMissingClasses
		description: 'There should have been some unloadable definitions'.
	loader := 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition new name: name)]
					on: CypressLoaderMissingClasses
					do: 
						[:ex |
						self
							assert: #('AnotherNonExistentClass' 'NonExistentClass' 'UnloadableClass')
							equals: ex requirementsMap keys asSortedCollection asArray.
						ex resume].
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 3;
		assert: (loader unloadable gather: [:each | each]) size equals: 3;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 2
%
category: 'utility'
set compile_env: 0
method: CypressLoaderTest
erroneousClassDefinitions

	^{
		CypressClassDefinition
			name: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'
			superclassName: 'CypressMockBasic'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #()
			classVarNames: #()
			comment: 'This class tries to define an instance variable which already exists in the superclass.'.
	}
%
category: 'utility'
set compile_env: 0
method: CypressLoaderTest
unloadableDefinitions

	^{
		CypressClassDefinition
			name: 'UnloadableClass'
			superclassName: 'NonExistentClass'
			category: 'Cypress-Mocks'
			instVarNames: #()
			classInstVarNames: #()
			classVarNames: #()
			comment: 'This class depends on a class that is intended to be missing.'.
		CypressMethodDefinition
			className: 'UnloadableClass'
			classIsMeta: false
			selector: 'unloadable'
			category: 'accessing'
			source: 'unloadable
	"This method cannot be loaded because the class cannot be."'.
		CypressMethodDefinition
			className: 'AnotherNonExistentClass'
			classIsMeta: false
			selector: 'unloadable'
			category: 'accessing'
			source: 'unloadable
	"This method cannot be loaded because the class cannot be."'.
	}
%

! Remove existing behavior from CypressPatchTest
doit
CypressPatchTest removeAllMethods.
CypressPatchTest class removeAllMethods.
%
! ------------------- Class methods for CypressPatchTest
! ------------------- Instance methods for CypressPatchTest
category: 'tests'
set compile_env: 0
method: CypressPatchTest
testDictionaryOfPatchOperations
	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"

	| dict |
	dict := Dictionary new.
	self baseTargetPatch do: [:each | 
		dict at: each put: each ].
	self baseTargetPatch do: [:each | 
		self assert: (dict at: each) equals: each ].
%
category: 'tests'
set compile_env: 0
method: CypressPatchTest
testPatch

	| baseSnapshot targetSnapshot patch operations expected |
	baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
	targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
	patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
	operations := patch operations.
	expected := self baseTargetPatch asArray.
	self assert: operations size equals: expected size.
	operations do: [:each | self assert: (expected includes: each)]
%
category: 'tests'
set compile_env: 0
method: CypressPatchTest
testPatchOperationEquality

	| className modification removal addition |
	className := 'CypressMockBasic'.
	modification := CypressModification 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	name := aString') 
			to: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	"changed method"
	name := aString').
	self assert: modification equals: modification.
	removal := CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: removal equals: removal.
	addition := CypressAddition
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: addition equals: addition.
%

! Remove existing behavior from CypressSnapshotTest
doit
CypressSnapshotTest removeAllMethods.
CypressSnapshotTest class removeAllMethods.
%
! ------------------- Class methods for CypressSnapshotTest
! ------------------- Instance methods for CypressSnapshotTest
category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testExtensionsOnlySnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks-Extensions'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	self
		assert: packageDefinitions size equals: 1;
		assert: packageDefinitions first selector equals: 'isCypressMockBasic';
		assert: packageDefinitions first className equals: 'Object'
%
category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testSnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions
		do: [:def | self assert: (expectedDefinitions includes: def)]
%
category: 'tests'
set compile_env: 0
method: CypressSnapshotTest
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray equals: expectedDefinitions asArray
%

! Remove existing behavior from CypressStructureTest
doit
CypressStructureTest removeAllMethods.
CypressStructureTest class removeAllMethods.
%
! ------------------- Class methods for CypressStructureTest
! ------------------- Instance methods for CypressStructureTest
category: 'private'
set compile_env: 0
method: CypressStructureTest
compileJSON: aJsonString

	^CypressJsonParser parse: aJsonString
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name equals: 'CypressMockBasic'.
	self deny: classStructure isClassExtension description: 'Class structure should not have been an extension'.
	self assert: classStructure comment equals: 'This mock contains basic class and instance method selectors'.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 5.
	self assert: (classProperties at: 'instvars') equals: #('name').
	self assert: (classProperties at: 'classinstvars') equals: #('current').
	self assert: (classProperties at: 'name') equals: 'CypressMockBasic'.
	self assert: (classProperties at: 'super') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 4.
	self assert: classStructure classMethods size equals: 3.
	classStructure := packageStructure extensions first.
	self assert: classStructure name equals: 'Object'.
	self assert: classStructure isClassExtension description: 'Class structure should have been an extension'.
	self assert: classStructure comment equals: ''.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 1.
	self assert: (classProperties at: 'name') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 1.
	self assert: classStructure classMethods size equals: 0.
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testJson
	"Let's compile the JSON without errors"

	self compileJSON: self basePackageStructureJson
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureFromJson

	| packageStructure classStructure classProperties |
	packageStructure := CypressPackageStructure fromJson: self basePackageStructureJson.
	self assert: packageStructure name equals: 'Cypress-Mocks.package'.
	self assert: packageStructure packageName equals: 'Cypress-Mocks'.
	self assert: packageStructure properties isEmpty description: 'Properties should have been empty'.
	self assert: packageStructure extensions size equals: 1.
	self assert: packageStructure classes size equals: 1.
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureFromPackage

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition for ', def printString, ' did not match expected ones']
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureSnapshot

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromJs: (self compileJSON: self basePackageStructureJson).
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition was not as expected']
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPackageStructureToJson
    | packageStructure stream string expected x y |
    packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
    stream := WriteStream on: String new.
    packageStructure writeJsonOn: stream.
    string := stream contents withUnixLineEndings.
    expected := self basePackageStructureJson withUnixLineEndings.
    1 to: string size do: [ :i | 
        (i > expected size or: [ (string at: i) ~= (expected at: i) ])
            ifTrue: [ 
                x := string copyFrom: (i - 25 max: 1) to: (i + 25 min: string size).
                y := expected copyFrom: ((i - 25 max: 1) min: expected size) to: (i + 25 min: expected size).
                Array with: x with: y	"halt" ] ].
    self assert: expected equals: string
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryRead

	| propertyDictionary phoneNumbers |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	self assert: (propertyDictionary at: 'name') equals: 'John Smith'.
	self assert: (propertyDictionary at: 'age') equals: 25.
	self assert: (propertyDictionary at: 'registered') description: '"registered" property should have been true'.
	phoneNumbers := propertyDictionary at: 'phoneNumber'.
	self assert: phoneNumbers size equals: 2.
	self assert: ((phoneNumbers at: 1) at: 'number') equals: '212 555-1234'.
	self assert: ((phoneNumbers at: 2) at: 'number') equals: '646 555-4567'.
%
category: 'tests'
set compile_env: 0
method: CypressStructureTest
testPropertyDictionaryWrite
    | propertyDictionary stream x |
    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
    stream := WriteStream on: String new.
    propertyDictionary writeCypressJsonOn: stream indent: 0.
    self assert: (x := stream contents withUnixLineEndings) equals: self sampleJson withUnixLineEndings
%
