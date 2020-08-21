! Class Declarations
! Generated file, do not Edit

doit
(Error
	subclass: 'RwTonelParseError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tonel-Core';
		comment: 'I''m a parsing error. 
I happen whenever the parsing of a tonel file is broken in someway.';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'RwTonelParseRequireMethodCategoryNotification'
	instVarNames: #( className isMeta selector )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tonel-Core';
		comment: 'The receiver is signalled when the RwTonelParser is about to create a method definition with no method category defined.

If the caller wants to continue, #resume: the notification with the value of the category to be used.

If unhandled,  a RwTonelParseError is signalled.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwTonelParser'
	instVarNames: #( packageReader stream lastSelectorParsed )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tonel-Core';
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
	subclass: 'RwTopazTonelReader'
	instVarNames: #( environmentId )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tonel-Core';
		comment: 'Support class for topaz `tfile` and `tmethod` commands.

`tmethod`
	Read and compile a single tonal format method from a given string

`tfile`
	Read a single tonel format class from a file and compile the methods within that file. Definition/redefinition of the class not implemented yet.';
		immediateInvariant.
true.
%

! Class implementation for 'RwTonelParseRequireMethodCategoryNotification'

!		Class methods for 'RwTonelParseRequireMethodCategoryNotification'

category: 'instance creation'
classmethod: RwTonelParseRequireMethodCategoryNotification
className: className isMeta: isMeta selector: selector
	^ self new
		className: className;
		isMeta: isMeta;
		selector: selector;
		yourself
%

!		Instance methods for 'RwTonelParseRequireMethodCategoryNotification'

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
className
	^className
%

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
className: object
	className := object
%

category: 'Handling'
method: RwTonelParseRequireMethodCategoryNotification
defaultAction
	"handle and resume: with the desired method category to be used to avoid error"

	RwTonelParseError
		signal:
			'Missing method category for ' , self className
				,
					(self isMeta
						ifTrue: [ ' class >> ' ]
						ifFalse: [ ' >> ' ]) , self selector
%

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
isMeta
	^isMeta
%

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
isMeta: object
	isMeta := object
%

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
selector
	^selector
%

category: 'accessing'
method: RwTonelParseRequireMethodCategoryNotification
selector: object
	selector := object
%

! Class implementation for 'RwTonelParser'

!		Class methods for 'RwTonelParser'

category: 'accessing'
classmethod: RwTonelParser
lineEnding
  "Answer the os-specific line endings"

  ^ String with: Character lf
%

category: 'instance creation'
classmethod: RwTonelParser
on: aStream forReader: aTonelReader
	^ self new 
		stream: aStream;
		packageReader: aTonelReader;
		yourself
%

category: 'instance creation'
classmethod: RwTonelParser
onString: aString forReader: aTonelReader
  ^ self on: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'parsing'
classmethod: RwTonelParser
parseStream: aStream forReader: aTonelReader
	^ (self on: aStream forReader: aTonelReader)
		 start
%

category: 'parsing'
classmethod: RwTonelParser
parseString: aString forReader: aTonelReader
	^ self parseStream: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'accessing'
classmethod: RwTonelParser
readStreamClass

	^ ReadStreamPortable
%

category: 'accessing'
classmethod: RwTonelParser
writeStreamClass

	^ WriteStreamPortable
%

!		Instance methods for 'RwTonelParser'

category: 'private'
method: RwTonelParser
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
method: RwTonelParser
comment
	| result ch eatNext |
	
	result := String new writeStreamPortable.

	eatNext := false.
	stream next = $" ifFalse: [ RwTonelParseError signal: 'Can''t parse comment' ].	
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
method: RwTonelParser
definitionForType: aString
  ^ self packageReader definitionForType: aString
%

category: 'parsing'
method: RwTonelParser
document
	^ { 
	self typeDef.
	self methodDefList.
	 } 
	select: [:each | each notNil ]
%

category: 'private'
method: RwTonelParser
extractSelector: aString
	| separators selectorStream keywords |
	
	separators := { 
		Character space. 
		Character tab. 
		Character lf. 
		Character newPage. 
		Character cr. 
		$:}.

	keywords := Array new writeStreamPortable.
	selectorStream := aString readStream.
	[ selectorStream atEnd ]
	whileFalse: [ | word ch |
		word := String new writeStreamPortable.
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
method: RwTonelParser
isEnter: aCharacter
	^ #(13 10) includes: aCharacter asciiValue
%

category: 'testing'
method: RwTonelParser
isSeparator: aCharacter 
	^ aCharacter isSeparator
%

category: 'parsing'
method: RwTonelParser
metadata
	| result ch count |
	
	result := String new writeStreamPortable.

	count := 0.
	stream peek = ${ ifFalse: [ RwTonelParseError signal: 'Can''t parse metadata' ].	
	[ stream atEnd not ]
	whileTrue: [ 
		ch := stream next.
		result nextPut: ch.
		ch = ${ ifTrue: [ count := count +1 ].
		ch = $} ifTrue: [ count := count -1 ].
		count = 0 ifTrue: [ ^ STON fromString: result contents ]].

	RwTonelParseError signal: 'Can''t parse metadata'
%

category: 'parsing'
method: RwTonelParser
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
method: RwTonelParser
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
	| result char prevChar comment string count startPos |
	
	result := self class writeStreamClass on: String new.

	comment := false.
	string := false.
	prevChar := nil.
	count := 0.
        startPos := stream position .
        "startBody := stream peek: 300 ." "uncomment for debugging parse problems"
	stream peek = $[ ifFalse: [ RwTonelParseError signal: 'Can''t parse method body' ].
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

	RwTonelParseError signal: 'Can''t parse method body'
%

category: 'parsing'
method: RwTonelParser
methodDef

	| methodDef |
	self methodDef: [:isMeta :mDef |
		methodDef :=  mDef.
		"skip possible spaces at the end"
		self separator ].
	^methodDef
%

category: 'parsing'
method: RwTonelParser
methodDef: aBlock
  | ar def offset |
  ar := {
    self separator.
    self try: [ self metadata ].
    self separator.
    [ offset := stream position . self method ] value .
    self methodBody
  }.
  (def := self newMethodDefinitionFrom: ar )
    offset: offset
    inFile: stream wrappedStreamName .

  aBlock
    value: ar fourth first second notNil
    value: def
%

category: 'parsing'
method: RwTonelParser
methodDefList
	| result classStream instanceStream |
	self separator. "to arrive to the end of the file in case there are no methods"
	result := { {}. {} }.
	classStream := (result at: 1) writeStreamPortable.
	instanceStream := (result at: 2) writeStreamPortable.
	[
		[ stream atEnd ]
			whileFalse: [ 
				self methodDef: [:isMeta :mDef |
					isMeta
						ifTrue: [ classStream nextPut: mDef ]
						ifFalse: [ instanceStream nextPut: mDef ].
					"skip possible spaces at the end"
					self separator ]
			] 
  ] on: (RwTonelParseError,STONReaderError,STONWriterError) do:[:ex | 
    lastSelectorParsed ifNotNil:[ | str |
      str := ex details ifNil:[ '' ].
      ex details: str, ', last method parsed: ', lastSelectorParsed printString
    ].
    ex pass 
  ].
  ^ result
%

category: 'private factory'
method: RwTonelParser
newMethodDefinitionFrom: anArray
	| metadata className meta selector source categ |
	metadata := anArray second ifNil: [ Dictionary new ].
	className := anArray fourth first first.
	(Metaclass3 _validateNewClassName: className asSymbol)
		ifFalse: [ self error: 'Invalid class name ' , className printString ].
	meta := anArray fourth first second notNil.
	selector := self extractSelector: anArray fourth second trimBoth.
	source := String
		streamContents: [ :s | 
			s << anArray fourth second.
			anArray fifth ifNotEmpty: [ :src | s << src ] ].

	categ := metadata
		at: #'category'
		ifAbsent: [ 
			"to avoid error, resume with default category string"
			(RwTonelParseRequireMethodCategoryNotification
				className: className
				isMeta: meta
				selector: selector) signal ].

	^ self packageReader
		newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: categ
		source: source
%

category: 'private factory'
method: RwTonelParser
newTypeDefinitionFrom: anArray
	^ self packageReader newTypeDefinitionFrom: anArray
%

category: 'accessing'
method: RwTonelParser
packageReader
	^ packageReader
%

category: 'accessing'
method: RwTonelParser
packageReader: aPackageReader 
	packageReader := aPackageReader
%

category: 'private'
method: RwTonelParser
removeFrom: aString enclosingStart: startChar end: endChar
	^ self 
		removeFrom: aString 
		enclosingStart: startChar 
		end: endChar
		clean: #both
%

category: 'private'
method: RwTonelParser
removeFrom: aString enclosingStart: startChar end: endChar clean: cleanSymbol
  "cleanSymbol can be #left, #rigth and #both"

  | result stop ch start end |
  result := self class readStreamClass on: aString trimBoth.
  result peek = startChar
    ifFalse: [ RwTonelParseError signal: 'I cannot remove enclosing start' ].
  result skip: 1.
  (#(#'both' #'left') includes: cleanSymbol)
    ifTrue: [ 
      stop := self class lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: 1 ] ].
  start := result position.
  result setToEnd.
  result skip: -1.
  result peek = endChar
    ifFalse: [ RwTonelParseError signal: 'I cannot remove enclosing end' ].
  result skip: -1.
  (#(#'both' #'right') includes: cleanSymbol)
    ifTrue: [ 
      stop := self class lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: -1 ] ].
  end := result position.
  ^ result originalContents copyFrom: start + 1 to: end + 1
%

category: 'parsing'
method: RwTonelParser
separator
	[ stream atEnd not and: [ self isSeparator: stream peek ] ]
	whileTrue: [ stream next ].
	^ nil
%

category: 'parsing'
method: RwTonelParser
shebang
	"look for a '#!' in first two character position and skip to next line if present"

	(stream peekFor: $#) ifFalse: [ ^ nil ].	
	(stream peekFor: $!) ifFalse: [ ^ nil ].
	^ stream  upTo: Character lf.
%

category: 'accessing'
method: RwTonelParser
start
	^ self document
%

category: 'accessing'
method: RwTonelParser
stream: aStream 
	stream := aStream
%

category: 'private parsing'
method: RwTonelParser
try: aBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue | parsedValue ] 
		onFailure: [ nil ]
%

category: 'private parsing'
method: RwTonelParser
try: aBlock onFailure: failureBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue |  parsedValue ] 
		onFailure: failureBlock
%

category: 'private parsing'
method: RwTonelParser
try: aBlock onSuccess: successBlock
	^ self 
		try: aBlock 
		onSuccess: successBlock 
		onFailure: [ nil ]
%

category: 'private parsing'
method: RwTonelParser
try: aBlock onSuccess: successBlock onFailure: failureBlock
	| pos |
	
	pos := stream position.
	[ ^ successBlock value: aBlock value ]
	on: RwTonelParseError 
	do: [ :e | 
		stream position: pos.
		^ failureBlock value ]. 
	
%

category: 'parsing'
method: RwTonelParser
type
	self try: [ self word: 'Class' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Trait' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Extension' ] onSuccess: [ :word | ^ word  ].
	
	"at end"
	RwTonelParseError signal: 'Can''t parse type.'	
%

category: 'parsing'
method: RwTonelParser
typeDef
	| shebang |
	shebang := self shebang. "ignore shebang on first line of file if present"
	^ self newTypeDefinitionFrom: { 
		self separator.
		self try: [ self comment ]. 
		self separator. 
		self type. 
		self separator. 
		self try: [ 
			| typeMetadata normalizedMetadata |
			typeMetadata := self metadata.
			normalizedMetadata := Dictionary new.
			typeMetadata keysAndValuesDo: [:key :value |
				normalizedMetadata at: key asLowercase asSymbol put: value ].
			normalizedMetadata at: #shebang put: shebang.
			normalizedMetadata ] 
	}
%

category: 'private parsing'
method: RwTonelParser
untilExcluding: aCollection
	| result |
	result := stream upToAll: aCollection.
	stream position: stream position - aCollection size.
	^ result
%

category: 'private parsing'
method: RwTonelParser
untilIncluding: aCollection
	^ stream upToAll: aCollection
%

category: 'private parsing'
method: RwTonelParser
word: aString
	| result |
	result := stream next: aString size.
	result = aString
		ifFalse: [ RwTonelParseError signal: 'Can''t parse ', aString ].
	^ result
%

! Class implementation for 'RwTopazTonelReader'

!		Class methods for 'RwTopazTonelReader'

category: 'instance creation'
classmethod: RwTopazTonelReader
forEnvironmentId: environmentId
	"Create a new instance of the receiver that will compile methods using environmentId"

	^ self new
		environmentId: environmentId;
		yourself
%

category: 'topaz support'
classmethod: RwTopazTonelReader
topazCompileTonelMethod: aString
	"Read and compile a single tonal format method from a given string.
		For topaz TMETHOD command"

	^ self topazCompileTonelMethod: aString envId: 0
%

category: 'topaz support'
classmethod: RwTopazTonelReader
topazCompileTonelMethod: aString envId: envId
	"Read and compile a single tonal format method (category plush method block) from a given string.
		For topaz TMETHOD command"

	| strm parser warnStr |
	strm := ReadStreamPortable on: aString.

	parser := RwTonelParser on: strm forReader: (self forEnvironmentId: envId).

	[ parser methodDef ]
		on: CompileWarning
		do: [ :ex | 
			warnStr := ex warningString.
			ex resume ].
	^ warnStr	"nil if no warnings"
%

category: 'topaz support'
classmethod: RwTopazTonelReader
topazReadTonelFile: filePath
	"Read a single tonel format class from a file and compile the methods within that file. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	^ self topazReadTonelFile: filePath envId: 0
%

category: 'topaz support'
classmethod: RwTopazTonelReader
topazReadTonelFile: filePath envId: envId
	"Read a single tonel format class from a file and compile the methods within that file. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	| gsfile stream |
	gsfile := GsFile openReadOnServer: filePath.
	stream := ReadStreamPortable on: gsfile contents.
	gsfile close.
	[ self topazReadTonelStream: stream envId: envId ]
		on: STONReaderError , RwTonelParseError
		do: [ :ex | 
			ex addText: (self _lineNumberStringForOffset: stream position fileName: filePath).
			ex pass ]
%

category: 'topaz support'
classmethod: RwTopazTonelReader
topazReadTonelStream: tonelStream envId: envId
	"Read a single tonel format class from a stream and compile the methods on that stream. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	RwTonelParser
		parseStream: tonelStream
		forReader: (self forEnvironmentId: envId)
%

category: 'private'
classmethod: RwTopazTonelReader
_lineNumberStringForOffset: offset fileName: fName
	| res |
	res := '  (Unable to determine line number)'.
	[ 
	| buf lf lNum gsfile |
	gsfile := GsFile openReadOnServer: fName.
	buf := gsfile contents.
	gsfile close.
	buf size > offset
		ifTrue: [ buf size: offset ].
	lNum := 1 + (buf occurrencesOf: (lf := Character lf)).
	res := '' , lf , ' near line ' , lNum asString , lf , ' in file ' , fName ]
		on: Error
		do: [ :ex | 
			"ignore"
			 ].
	^ res
%

!		Instance methods for 'RwTopazTonelReader'

category: 'accessing'
method: RwTopazTonelReader
environmentId
	^ environmentId ifNil: [ environmentId := 0 ]
%

category: 'accessing'
method: RwTopazTonelReader
environmentId: object
	environmentId := object
%

category: 'tonel parser interface'
method: RwTopazTonelReader
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
	| behavior symbolList |
	symbolList := GsCurrentSession currentSession symbolList.
	behavior := symbolList objectNamed: className asSymbol.
	meta
		ifTrue: [ behavior := behavior class ].
	behavior
		compileMethod: source
		dictionaries: symbolList
		category: category
		environmentId: self environmentId
%

category: 'tonel parser interface'
method: RwTopazTonelReader
newTypeDefinitionFrom: anArray
	"class definition/redefinition not supported"
%

category: 'method definition'
method: RwTopazTonelReader
offset: anInteger inFile: aFileName
	"message sent to method definitions ... avoid MNU"
%

! Class extensions for 'CharacterCollection'

!		Instance methods for 'CharacterCollection'

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
endsWith: suffix

	"Answer whether the tail end of the receiver is the same as suffix.
	 The comparison is case-sensitive."

	| ofs |
	suffix size == 0
		ifTrue: [ ^ false ].
	(ofs := self size - suffix size) < 0
		ifTrue: [ ^ false ].
	^ self at: ofs + 1 equals: suffix	"
  'Elvis' endsWith: 'vis'
  'Elvis' endsWith: ''
"
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]
%

category: '*rowan-tonel-gemstone-kernel'
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

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
putOn: aStream

	^ aStream nextPutAll: self
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimBoth

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimBoth: aBlock

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimLeft

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aBlock

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aLeftBlock right: aRightBlock

	"Trim characters satisfying the condition given in aLeftBlock from the left side and aRightBlock from the right sides of the receiving string."

	| left right |
	left := 1.
	right := self size.
	[ left <= right and: [ aLeftBlock value: (self at: left) ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ aRightBlock value: (self at: right) ] ]
		whileTrue: [ right := right - 1 ].
	^ self copyFrom: left to: right
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimRight

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimRight: aBlock

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
withLineEndings: lineEndingString
	| stream |
	stream := nil.
	self
		lineIndicesDo: [ :start :endWithoutDelimiters :end | 
			(stream isNil and: [ endWithoutDelimiters ~= end ])
				ifTrue: [ 
					((self copyFrom: endWithoutDelimiters + 1 to: end)
						_unicodeEqual: lineEndingString)
						ifFalse: [ 
							stream := WriteStreamPortable with: self copy.
							stream position: start - 1 ] ].
			stream
				ifNotNil: [ 
					stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
					endWithoutDelimiters = end
						ifFalse: [ stream nextPutAll: lineEndingString ] ] ].
	^ stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ] ]
%

! Class extensions for 'Collection'

!		Instance methods for 'Collection'

category: '*rowan-tonel-gemstone-kernel'
method: Collection
asDictionary

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
flattened
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	^ Array streamContents: [ :stream | self flattenOn: stream].
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
flattenOn: aStream

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
ifNotEmpty: aBlock

	^ self size == 0
		ifFalse: [ aBlock cull: self ]
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
select: selectBlock thenDo: doBlock
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock
%

! Class extensions for 'GsFile'

!		Instance methods for 'GsFile'

category: '*rowan-tonel-gemstone-kernel'
method: GsFile
<< items

 	items putOn: self.
	
	^ self
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*rowan-tonel-gemstone-kernel'
method: Object
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false
%

category: '*rowan-tonel-gemstone-kernel'
method: Object
putOn: aStream

	^ aStream nextPut: self
%

! Class extensions for 'PositionableStreamPortable'

!		Instance methods for 'PositionableStreamPortable'

category: '*rowan-tonel-gemstone-kernel'
method: PositionableStreamPortable
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
%

! Class extensions for 'SequenceableCollection'

!		Instance methods for 'SequenceableCollection'

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
allButLast

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
allButLast: n

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty
		or: [ self size < aSequenceableCollection size ])
		ifTrue: [ ^ false ].
	aSequenceableCollection
		withIndexDo: [ :each :index | 
			(self at: index) ~= each
				ifTrue: [ ^ false ] ].
	^ true
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
fifth

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
fourth

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
%

category: '*rowan-tonel-gemstone-kernel'
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

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
putOn: aStream

	self do: [ :each | each putOn: aStream ]
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
sixth

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
third

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
%

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
withIndexDo: elementAndIndexBlock

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*rowan-tonel-gemstone-kernel'
method: Stream
<< items

	items putOn: self
%

