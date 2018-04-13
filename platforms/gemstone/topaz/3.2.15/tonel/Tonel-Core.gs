! Package: Tonel-Core


! Remove existing behavior from package Tonel-Core
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Tonel-Core'.
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
(Error
	subclass: 'TonelParseError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Core';
		comment: 'I''m a parsing error. 
I happen whenever the parsing of a tonel file is broken in someway.';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'TonelShouldIgnore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Core';
		comment: 'I''m a notification to say tonel writer that he should ignore a section. 
This tipically happens on a MCClassTraitDefinition, because it will be managed on MCTraitDefinition.

(see TonelWriter>>typeOf:)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'TonelParser'
	instVarNames: #( packageReader stream lastSelectorParsed )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Core';
		comment: 'I''m a parser for tonel files. 
I parse a class with the following format: 

Tonel spec
====

    [comment]
    type { typeDefinition }
    (
        [{ methodMetadata }]
        method [
            methodBody ] 
    )*


comment
---
"
comment string
"
is optional (but it should be there, in good design ;)

type
---
Class|Trait|Extension

typeDefinition
---
a STON file with class/trait/extension metadata

methodMetadata
---
a STON file with method metadata
is optional (but also, recommended)

method
---
method declaration as this: 

Class[ class] >> selector

methodBody 
---
the method body (we do not parse contents, that''s class builder task)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'TonelWriter'
	instVarNames: #( packageWriter )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(STONWriter
	subclass: 'TonelSTONWriter'
	instVarNames: #( aliases )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Tonel-Core';
		comment: 'I''m a modified STON writer to make tonel metadata look as we want.

- it accept aliasses for classes, so I can say OrderedDictionary -> nil (then I do not have an extra information I do not want). Btw, tonel needs to use ordered dictionaries instead plain dictionaries because output needs to be deterministic, and we want to control the order of attributes we publish.
- if dictionary has just one element, it prints it in just one line, to have a more compact view.';
		immediateInvariant.
true.
%

! Class Implementation for TonelParseError

! Class Implementation for TonelShouldIgnore

! Class Implementation for TonelParser

! ------------------- Class methods for TonelParser

category: 'instance creation'
classmethod: TonelParser
on: aStream forReader: aTonelReader
	^ self new 
		stream: aStream;
		packageReader: aTonelReader;
		yourself
%

category: 'instance creation'
classmethod: TonelParser
onString: aString forReader: aTonelReader
  ^ self on: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'parsing'
classmethod: TonelParser
parseStream: aStream forReader: aTonelReader
	^ (self on: aStream forReader: aTonelReader)
		 start
%

category: 'parsing'
classmethod: TonelParser
parseString: aString forReader: aTonelReader
	^ self parseStream: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'Topaz support'
classmethod: TonelParser
_compileForTopaz: aString envId: envId
  "aString is the topaz text object for topaz TMETHOD command"
| strm parser catDict category methInfo methSrc symList clsName cls
  warnStr  |
strm :=  ReadStreamPortable on: aString  .
(parser := self new) stream: strm .

parser separator ifNotNil:[ Error signal:'expected parser separator to be nil'].

catDict :=  (parser try: [ parser metadata ]).
catDict ifNil:[ Error signal:'Category dictionary not found'. ].
category := catDict at: #category .

parser separator ifNotNil:[ Error signal:'expected second parser separator to be nil'].

methInfo :=  parser method.
clsName := Symbol _existingWithAll: ((methInfo at: 1) at: 1) .

methSrc :=  (methInfo at: 2)"keywords+args" ,  parser methodBody .
symList := System myUserProfile symbolList .
cls := (symList resolveSymbol: clsName) value .
((methInfo at: 1) at: 2) ifNotNil:[:classWord |
  classWord = 'class' ifTrue:[ cls := cls class ]
      ifFalse:[ Error signal:'unrecognized ' , classWord asString, ' after >>'].
].
[ cls compileMethod: methSrc dictionaries: symList 
               category: category  
               environmentId: envId .
] on: CompileWarning do:[:ex | 
   warnStr := ex warningString .
   ex resume 
].
^ warnStr "nil if no warnings"
% 



! ------------------- Instance methods for TonelParser

category: 'private'
method: TonelParser
cleanSelector: aString
	"BEWARE: I'm doing some heave assumptions here: I'm removing just ONE space (in case there 
	 is one) because I expect this to be a file generated by tonel, and tonel adds one space 
	 before start with the method body to make the format more readable. 
	 But of course this is not very good :("
	^ (aString last = Character space
		ifTrue: [ aString allButLast ]
		ifFalse: [ aString ]) 
		trimLeft
%

category: 'parsing'
method: TonelParser
comment
	| result ch eatNext |
	
	result := String new writeStream.

	eatNext := false.
	stream next = $" ifFalse: [ TonelParseError signal: 'Can''t parse comment' ].	
	[ stream atEnd not 
		and: [ 
				(ch := stream next) ~= $" 
				or: [ eatNext := (stream peek = $") ] ] ]
	whileTrue: [ 
		result nextPut: ch.
		eatNext ifTrue: [ 
			stream skip: 1.
			eatNext := false ] ].
	
	^ self 
		removeFrom: '"',result contents,'"' 
		enclosingStart: $" 
		end: $"
%

category: 'private factory'
method: TonelParser
definitionForType: aString
  ^ self packageReader definitionForType: aString
%

category: 'parsing'
method: TonelParser
document
	^ { 
	self typeDef.
	self methodDefList.
	 } 
	flattened
	select: [:each | each notNil ]
%

category: 'private'
method: TonelParser
extractSelector: aString
	| separators selectorStream keywords |
	
	separators := { 
		Character space. 
		Character tab. 
		Character lf. 
		Character newPage. 
		Character cr. 
		$:}.

	keywords := Array new writeStream.
	selectorStream := aString readStream.
	[ selectorStream atEnd ]
	whileFalse: [ | word ch |
		word := String new writeStream.
		[ selectorStream atEnd not and: [ (separators includes: (ch := selectorStream next)) not ] ]
		whileTrue: [ word nextPut: ch ].
		ch = $: ifTrue: [ word nextPut: ch ]. 
		word contents trimBoth ifNotEmpty: [ :v | keywords nextPut: v ] ].
	keywords := keywords contents.

	^ (keywords size <= 2 
		ifTrue: [ keywords first]
		ifFalse: [ ('' join: (keywords pairsCollect: [ :keyword :argument | keyword ])) ])
		asSymbol
%

category: 'testing'
method: TonelParser
isEnter: aCharacter
	^ #(13 10) includes: aCharacter asciiValue
%

category: 'testing'
method: TonelParser
isSeparator: aCharacter 
	^ aCharacter isSeparator
%

category: 'parsing'
method: TonelParser
metadata
	| result ch count |
	
	result := String new writeStream.

	count := 0.
	stream peek = ${ ifFalse: [ TonelParseError signal: 'Can''t parse metadata' ].	
	[ stream atEnd not ]
	whileTrue: [ 
		ch := stream next.
		result nextPut: ch.
		ch = ${ ifTrue: [ count := count +1 ].
		ch = $} ifTrue: [ count := count -1 ].
		count = 0 ifTrue: [ ^ STON fromString: result contents ]].

	TonelParseError signal: 'Can''t parse metadata'
%

category: 'parsing'
method: TonelParser
method
	| type selector |
	
	type := self untilIncluding: '>>'.
	selector := self cleanSelector: (self untilExcluding: '[').
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
  lastSelectorParsed := selector .
	^ { 
		type.
		selector.
	}
%

category: 'parsing'
method: TonelParser
methodBody
	"I read a methodbody (what is inside [ ... ])
	 Since a method body can contain enclosing brackets we need to be sure we will skip them and
	 correctly read the method. For that, I have to take into account: 
		- I can mention [] in comments
		- I can mention [] in strings
		- I can use $[, $] 
		- I can have inner blocks
		- I can mention a comment of the form ""$"" or a comment of the form '$'
	 all that needs to be skipped "
	| result char prevChar comment string count |
	
	result := self class writeStreamClass on: String new.

	comment := false.
	string := false.
	prevChar := nil.
	count := 0.
	stream peek = $[ ifFalse: [ TonelParseError signal: 'Can''t parse method body' ].
	[ stream atEnd not ]
	whileTrue: [ 
		char := stream next.
		result nextPut: char.
		(char = $" and: [ string not and: [ prevChar ~= $$ or: [ comment ] ] ]) 
			ifTrue: [ comment := comment not ]. 
		(char = $' and: [ comment not and: [ prevChar ~= $$ or: [ string ] ] ]) 
			ifTrue: [ string := string not ]. 
		(comment or: [ string ]) ifFalse: [ 
			(char = $[ and: [  prevChar ~= $$ ]) ifTrue: [ count := count +1 ].
			(char = $] and: [ prevChar ~= $$ ]) ifTrue: [ count := count -1 ] ].
		count = 0 ifTrue: [ 
			^ self 
				removeFrom: result contents 
				enclosingStart: $[ 
				end: $]
				clean: #right ].
		prevChar := char ].

	TonelParseError signal: 'Can''t parse method body'
%

category: 'parsing'
method: TonelParser
methodDef
	^ self newMethodDefinitionFrom: { 
		self separator.
		self try: [ self metadata ]. 
		self separator. 
		self method. 
		self methodBody 
	}
%

category: 'parsing'
method: TonelParser
methodDefList
	| result |
	
	self separator. "to arrive to the end of the file in case there are no methods"
	result := Array new writeStream.
  [
	  [ stream atEnd ]
	  whileFalse: [ 
		  result nextPut: self methodDef .
		  "skip possible spaces at the end"
		  self separator 
      ].
  ] on: TonelParseError do:[:ex | 
     lastSelectorParsed ifNotNil:[
       GsFile gciLogServer:'Last selector parsed was: ', lastSelectorParsed printString .
     ].
     ex pass .
  ].
	^ result contents
%

category: 'private factory'
method: TonelParser
newMethodDefinitionFrom: anArray
	| metadata className meta selector source  |
	
	metadata := anArray second ifNil: [ Dictionary new ].
	className := anArray fourth first first.
	meta := anArray fourth first second notNil.
	selector := self extractSelector: anArray fourth second trimBoth.
	source := String streamContents: [ :s | 
		s << anArray fourth second.
		anArray fifth ifNotEmpty: [ :src | s << src ] ].

	^ self packageReader newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: (metadata at: #category ifAbsent: [ '' ]) 
		source: source
%

category: 'private factory'
method: TonelParser
newTypeDefinitionFrom: anArray
	^ self packageReader newTypeDefinitionFrom: anArray
%

category: 'accessing'
method: TonelParser
packageReader
	^ packageReader
%

category: 'accessing'
method: TonelParser
packageReader: aPackageReader 
	packageReader := aPackageReader
%

category: 'private'
method: TonelParser
removeFrom: aString enclosingStart: startChar end: endChar
	^ self 
		removeFrom: aString 
		enclosingStart: startChar 
		end: endChar
		clean: #both
%

category: 'private'
method: TonelParser
removeFrom: aString enclosingStart: startChar end: endChar clean: cleanSymbol
  "cleanSymbol can be #left, #rigth and #both"

  | result stop ch start end |
  result := self class readStreamClass on: aString trimBoth.
  result peek = startChar
    ifFalse: [ TonelParseError signal: 'I cannot remove enclosing start' ].
  result skip: 1.
  (#(#'both' #'left') includes: cleanSymbol)
    ifTrue: [ 
      stop := TonelWriter lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: 1 ] ].
  start := result position.
  result setToEnd.
  result skip: -1.
  result peek = endChar
    ifFalse: [ TonelParseError signal: 'I cannot remove enclosing end' ].
  result skip: -1.
  (#(#'both' #'right') includes: cleanSymbol)
    ifTrue: [ 
      stop := TonelWriter lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: -1 ] ].
  end := result position.
  ^ result originalContents copyFrom: start + 1 to: end + 1
%

category: 'parsing'
method: TonelParser
separator
	[ stream atEnd not and: [ self isSeparator: stream peek ] ]
	whileTrue: [ stream next ].
	^ nil
%

category: 'accessing'
method: TonelParser
start
	^ self document
%

category: 'accessing'
method: TonelParser
stream: aStream 
	stream := aStream
%

category: 'private parsing'
method: TonelParser
try: aBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue | parsedValue ] 
		onFailure: [ nil ]
%

category: 'private parsing'
method: TonelParser
try: aBlock onFailure: failureBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue |  parsedValue ] 
		onFailure: failureBlock
%

category: 'private parsing'
method: TonelParser
try: aBlock onSuccess: successBlock
	^ self 
		try: aBlock 
		onSuccess: successBlock 
		onFailure: [ nil ]
%

category: 'private parsing'
method: TonelParser
try: aBlock onSuccess: successBlock onFailure: failureBlock
	| pos |
	
	pos := stream position.
	[ ^ successBlock value: aBlock value ]
	on: TonelParseError 
	do: [ :e | 
		stream position: pos.
		^ failureBlock value ]. 
	
%

category: 'parsing'
method: TonelParser
type
	self try: [ self word: 'Class' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Trait' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Extension' ] onSuccess: [ :word | ^ word  ].
	
	"at end"
	TonelParseError signal: 'Can''t parse type.'	
%

category: 'parsing'
method: TonelParser
typeDef
	^ self newTypeDefinitionFrom: { 
		self separator.
		self try: [ self comment ]. 
		self separator. 
		self type. 
		self separator. 
		self try: [ self metadata ] 
	} 
	
		
%

category: 'private parsing'
method: TonelParser
untilExcluding: aCollection
	| result |
	result := stream upToAll: aCollection.
	stream position: stream position - aCollection size.
	^ result
%

category: 'private parsing'
method: TonelParser
untilIncluding: aCollection
	^ stream upToAll: aCollection
%

category: 'private parsing'
method: TonelParser
word: aString
	| result |
	result := stream next: aString size.
	result = aString
		ifFalse: [ TonelParseError signal: 'Can''t parse ', aString ].
	^ result
%

! Class Implementation for TonelWriter

! ------------------- Class methods for TonelWriter

category: 'accessing'
classmethod: TonelWriter
classLabel
	^ 'Class'
%

category: 'accessing'
classmethod: TonelWriter
extensionLabel
	^ 'Extension'
%

category: 'instance creation'
classmethod: TonelWriter
on: aPackageWriter

	^ self new
		packageWriter: aPackageWriter;
		yourself
%

category: 'accessing'
classmethod: TonelWriter
traitLabel
	^ 'Trait'
%

! ------------------- Instance methods for TonelWriter

category: 'private'
method: TonelWriter
classNameFor: aMethodDefinition parent: aClassDefinition
	aClassDefinition ifNil: [ ^ aMethodDefinition fullClassName ].
	^ aMethodDefinition classIsMeta
		ifFalse: [ aMethodDefinition className ]
		ifTrue: [ 
			aClassDefinition isTraitDefinition
				ifFalse: [aMethodDefinition className, ' class']
				ifTrue: [aMethodDefinition className, ' classSide'] ]
%

category: 'private definitions'
method: TonelWriter
commentOf: aClassDefinition
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self newLine
%

category: 'accessing'
method: TonelWriter
definitions

   ^self packageWriter definitions
%

category: 'private'
method: TonelWriter
fileNameFor: aClassDefinition
	^ String streamContents: [ :stream | 
		stream 
			<< aClassDefinition className
			<< '.' << (self typeOf: aClassDefinition) asLowercase
			<< '.st'  ]
%

category: 'private testing'
method: TonelWriter
isClass: aClassDefinition
	^ self packageWriter isClass: aClassDefinition
%

category: 'private testing'
method: TonelWriter
isTrait: aClassDefinition
	^ self packageWriter isTrait: aClassDefinition
%

category: 'private definitions'
method: TonelWriter
methodDefinitionOf: aMethodDefinition
	^ self toSTON: (self class orderedDictionaryClass new 
		at: #category put: aMethodDefinition category asSymbol; 
		yourself)	
	
%

category: 'private'
method: TonelWriter
newLine
	 ^ self class lineEnding
%

category: 'accessing'
method: TonelWriter
packageWriter
  ^ packageWriter
%

category: 'accessing'
method: TonelWriter
packageWriter: anObject

   packageWriter := anObject
%

category: 'private'
method: TonelWriter
selectorIsComplete: keywords in: aString
	| start |
	
	start := 1.
	keywords do: [ :each | | index | 
		index := aString 
			findString: each 
			startingAt: start 
			caseSensitive: true.
		index = 0 ifTrue: [ ^ false ].
		start := index + each size ].
	^ true
%

category: 'private'
method: TonelWriter
skipComment: aStream
	"I assume I'm on top of the begining of a comment"
	aStream skip: 1.
	[ aStream atEnd not 
		and: [ aStream next ~= $" or: [ aStream peek = $" ] ] ]
	whileTrue.	
%

category: 'private'
method: TonelWriter
skipSeparators: aStream
	[ aStream peek isSeparator ]
	whileTrue: [ aStream skip: 1 ]. 
%

category: 'private'
method: TonelWriter
splitMethodSource: aMethodDefinition into: aBlock
	| keywords source declaration |
	
	keywords := aMethodDefinition selector asSymbol keywords.
	source := aMethodDefinition source readStream.
	"Skip spaces"
	(source peek isSeparator) ifTrue: [ self skipSeparators: source ].
	"Skip comments"
	(source peek = $") ifTrue: [ self skipComment: source ]. 
	"Parse declaration"
	declaration := String new writeStream.
	[ (self selectorIsComplete: keywords in: declaration originalContents) not 
		or: [ ':+-/\*~<>=@,%|&?!' includes: declaration contents trimRight last ] ]
	whileTrue: [ 
		"get separators"
		[ source atEnd not and: [ source peek isSeparator ] ]
			whileTrue: [ declaration nextPut: source next ].
		"take next word"
		[ source atEnd not and: [ source peek isSeparator not ] ]
			whileTrue: [ declaration nextPut: source next ] ].
	aBlock 
		value: (declaration contents trimLeft withLineEndings: self newLine)
		value: (source upToEnd withLineEndings: self newLine)
%

category: 'private'
method: TonelWriter
toSTON: anObject
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self newLine
%

category: 'private definitions'
method: TonelWriter
typeClassDefinitionOf: aClassDefinition
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition className asSymbol; 
		at: #superclass put: aClassDefinition superclassName asSymbol;
		yourself.

	aClassDefinition type = #normal ifFalse: [ 
		definition at: #type put: aClassDefinition type ].
	
	aClassDefinition hasTraitComposition ifTrue: [ 
		definition at: #traits put: aClassDefinition traitCompositionString ].
	
	aClassDefinition hasClassTraitComposition ifTrue: [ 
		definition at: #classTraits put: aClassDefinition classTraitCompositionString ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaries) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	definition 		
		at: #category put: aClassDefinition category asSymbol.
	
	^ self toSTON: definition
%

category: 'private definitions'
method: TonelWriter
typeDefinitionOf: aClassDefinition
	(self isTrait: aClassDefinition) 
		ifTrue: [ ^ self typeTraitDefinitionOf: aClassDefinition ].
	^ self typeClassDefinitionOf: aClassDefinition
%

category: 'private'
method: TonelWriter
typeOf: aClassDefinition
	(self isClass: aClassDefinition) ifTrue: [ ^ self class classLabel ].
	(self isTrait: aClassDefinition) ifTrue: [ ^ self class traitLabel ].

	TonelShouldIgnore signal
%

category: 'writing'
method: TonelWriter
writeClass: aClassDefinition
	[ 
		self packageWriter 
			writePackageFileNamed: (self fileNameFor: aClassDefinition) 
			do:  [ :aStream | 
				self writeClassDefinition: aClassDefinition on: aStream.
				self writeClassSideMethodDefinitions: aClassDefinition on: aStream.
				self writeInstanceSideMethodDefinitions: aClassDefinition on: aStream ] ]
	on: TonelShouldIgnore
	do: [ :e | self logCr: 'ignoring: ', aClassDefinition asString ]
%

category: 'private writing'
method: TonelWriter
writeClassDefinition: aClassDefinition on: aStream
	| nl |
	nl := self newLine.
	
	aClassDefinition hasComment 
		ifTrue: [ 
			aStream 
				<< '"' << nl
				<< (self commentOf: aClassDefinition) << nl
				<< '"' << nl ].
	aStream
		<< (self typeOf: aClassDefinition) 
		<< ' ' << (self typeDefinitionOf: aClassDefinition ) << nl
%

category: 'private writing'
method: TonelWriter
writeClassSideMethodDefinitions: aClassDefinition on: aStream
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]
%

category: 'writing'
method: TonelWriter
writeExtensionMethods: methods className: className

	| nl |
	nl := self newLine.
	self packageWriter
		writePackageFileNamed: className , '.extension.st'
		do: [ :s | 
			s << 'Extension '
				<< (self toSTON: {(#'name' -> className asSymbol)} asDictionary) << nl.
			((methods select: [ :m | m classIsMeta not ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ].
			((methods select: [ :m | m classIsMeta ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ] ]
%

category: 'private writing'
method: TonelWriter
writeInstanceSideMethodDefinitions: aClassDefinition on: aStream
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta not ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]

	
	
%

category: 'private writing'
method: TonelWriter
writeMethodDefinition: aMethodDefinition on: aStream
	^ self 
		writeMethodDefinition: aMethodDefinition 
		parent: nil 
		on: aStream
%

category: 'private writing'
method: TonelWriter
writeMethodDefinition: aMethodDefinition parent: aClassDefinition on: aStream
	| nl |
	
	nl := self newLine.
	self 
		splitMethodSource: aMethodDefinition 
		into: [ :methodDeclaration :methodBody | | fullClassName |
			fullClassName := self classNameFor: aMethodDefinition parent: aClassDefinition.
			aStream 
				<< nl 
				<< (self methodDefinitionOf: aMethodDefinition) << nl 
				<< fullClassName << ' >> ' << methodDeclaration 
				<< ' [' << methodBody << nl << ']' << nl ]
%

category: 'writing'
method: TonelWriter
writePackage: packageName
	self packageWriter 
		writePackageFileNamed: 'package.st'
		do:  [ :aStream | self writePackage: packageName on: aStream ]
%

category: 'writing'
method: TonelWriter
writePackage: packageName on: aStream

	aStream 
		<< 'Package ' 
		<< (self toSTON: { #name ->  packageName asSymbol } asDictionary) 
		<< self  newLine
%

! Class Implementation for TonelSTONWriter

! ------------------- Instance methods for TonelSTONWriter

category: 'accessing'
method: TonelSTONWriter
aliases
	^ aliases
%

category: 'accessing'
method: TonelSTONWriter
aliases: aDictionary
	aliases := aDictionary
%

category: 'private'
method: TonelSTONWriter
encodeKey: key value: value

	super encodeKey: key asSymbol
		value: (value isSymbol ifTrue: [value asString] ifFalse: [value])
%

category: 'writing'
method: TonelSTONWriter
encodeMap: pairs
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ].
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ] ].
	writeStream nextPut: $}
%

category: 'initialization'
method: TonelSTONWriter
initialize
	super initialize.
	self prettyPrint: true.
	aliases := { TonelWriter orderedDictionaryClass -> nil } asDictionary
%

category: 'accessing'
method: TonelSTONWriter
stonNameFor: aClass
	^ self aliases 
		at: aClass 
		ifAbsent: [ aClass stonName ]
%

category: 'writing'
method: TonelSTONWriter
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ self error: 'wrong object class for JSON mode' ].
	self with: anObject do: [
		(self stonNameFor: anObject class) ifNotNil: [ :stonName | 
			writeStream nextPutAll: stonName.
			self prettyPrintSpace ].
		block value ]
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: Tonel-Core


