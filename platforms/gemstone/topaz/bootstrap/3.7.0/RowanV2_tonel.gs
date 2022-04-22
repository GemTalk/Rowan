! Class Declarations
! Generated file, do not Edit

doit
(Error
	subclass: 'RwTonelParseError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'Rowan-Tonel-Core';
		comment: 'I''m a parsing error. 
I happen whenever the parsing of a tonel file is broken in someway.';
		immediateInvariant.
true.
%

removeallmethods RwTonelParseError
removeallclassmethods RwTonelParseError

doit
(Error
	subclass: 'STONReaderError'
	instVarNames: #( streamPosition )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONReaderError is the error/exception signalled by STONReader when illegal/incorrect input is seen. 
';
		immediateInvariant.
true.
%

removeallmethods STONReaderError
removeallclassmethods STONReaderError

doit
(Error
	subclass: 'STONWriterError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONWriterError is the error/exception signalled by STONWriter when illegal/incorrect input is seen. ';
		immediateInvariant.
true.
%

removeallmethods STONWriterError
removeallclassmethods STONWriterError

doit
(Notification
	subclass: 'RwTonelParseRequireMethodCategoryNotification'
	instVarNames: #( className isMeta selector )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'Rowan-Tonel-Core';
		comment: 'The receiver is signalled when the RwTonelParser is about to create a method definition with no method category defined.

If the caller wants to continue, #resume: the notification with the value of the category to be used.

If unhandled,  a RwTonelParseError is signalled.';
		immediateInvariant.
true.
%

removeallmethods RwTonelParseRequireMethodCategoryNotification
removeallclassmethods RwTonelParseRequireMethodCategoryNotification

doit
(Object
	subclass: 'RwTonelParser'
	instVarNames: #( packageReader stream lastSelectorParsed )
	classVars: #( Character_lf )
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
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

removeallmethods RwTonelParser
removeallclassmethods RwTonelParser

doit
(Object
	subclass: 'RwTopazTonelReader'
	instVarNames: #( environmentId )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
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

removeallmethods RwTopazTonelReader
removeallclassmethods RwTopazTonelReader

doit
(Object
	subclass: 'STON'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STON implements serialization and materialization using the Smalltalk Object Notation format.
 
S y n t a x

	value
	  primitive-value
	  object-value
	  reference
	  nil
	primitive-value
	  number
	  true
	  false
	  symbol
	  string
	object-value
	  object
	  map
	  list
	object
	  classname map
	  classname list
	reference
	  @ int-index-previous-object-value
	map
	  {}
	  { members }
	members
	  pair
	  pair , members
	pair
	  string : value
	  symbol : value
	  number : value
	list
	  []
	  [ elements ]
	elements
	  value 
	  value , elements
	string
	  ''''
	  '' chars ''
	chars
	  char
	  char chars
	char
	  any-printable-ASCII-character-
	    except-''-"-or-\
	  \''
	  \"
	  \\
	  \/
	  \b
	  \f
	  \n
	  \r
	  \t
	  \u four-hex-digits
	symbol
	  # chars-limited
	  # '' chars ''
	chars-limited
	  char-limited
	  char-limited chars-limited
	char-limited
	  a-z A-Z 0-9 - _ . /
	classname
	  uppercase-alpha-char alphanumeric-char
	number
	  int
	  int frac
	  int exp
	  int frac exp
	int
	  digit
	  digit1-9 digits 
	  - digit
	  - digit1-9 digits
	frac
	  . digits
	exp
	  e digits
	digits
	  digit
	  digit digits
	e
	  e
	  e+
	  e-
	  E
	  E+
	  E-
';
		immediateInvariant.
true.
%

removeallmethods STON
removeallclassmethods STON

doit
(Object
	subclass: 'STONReader'
	instVarNames: #( readStream objects classes unresolvedReferences stringStream allowComplexMapKeys stack )
	classVars: #( Character_lf Character_backspace Character_newPage Character_cr Character_tab )
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONReader materializes objects using the Smalltalk Object Notation format.

This parser is backwards compatible with standard JSON.';
		immediateInvariant.
true.
%

removeallmethods STONReader
removeallclassmethods STONReader

doit
(Object
	subclass: 'STONReference'
	instVarNames: #( index )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONReference holds a forward reference to another object during materialization.
';
		immediateInvariant.
true.
%

removeallmethods STONReference
removeallclassmethods STONReference

doit
(Object
	subclass: 'STONStreamWriter'
	instVarNames: #( writer first )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONStreamWriter helps in streaming writing STON representations.
This is an abstract class.';
		immediateInvariant.
true.
%

removeallmethods STONStreamWriter
removeallclassmethods STONStreamWriter

doit
(STONStreamWriter
	subclass: 'STONListWriter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONArrayWriter helps in writing array based STON representations.
';
		immediateInvariant.
true.
%

removeallmethods STONListWriter
removeallclassmethods STONListWriter

doit
(STONListWriter
	subclass: 'STONShortListWriter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONShortArrayWriter helps in writing short array based STON representations.
';
		immediateInvariant.
true.
%

removeallmethods STONShortListWriter
removeallclassmethods STONShortListWriter

doit
(STONStreamWriter
	subclass: 'STONMapWriter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONDictionaryWriter helps in writing dictionary based STON representations.';
		immediateInvariant.
true.
%

removeallmethods STONMapWriter
removeallclassmethods STONMapWriter

doit
(Object
	subclass: 'STONWriter'
	instVarNames: #( writeStream prettyPrint newLine jsonMode referencePolicy level objects )
	classVars: #( STONCharacters STONSimpleSymbolCharacters )
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #( #logCreation )
)
		category: 'STON-Core';
		comment: 'STONWriter serializes objects using the Smalltalk Object Notation format. 

Customization options are:

- prettyPrint <Boolean> default is false
	if true, produce pretty printed output
- jsonMode <Boolean> default is false
	if true, the follow changes occur
	- strings are delimited with double quotes
	- nil is encoded as null
	- symbols are treated as strings
	- only STON listClass and STON mapClass instances are allowed as composite objects
	it is wise to also use either #error or #ignore as referencePolicy to avoid references
- referencePolicy <#normal|#ignore|#error> default is #normal
	if #normal, track and count object references and use references to implement sharing and break cycles
	if #error, track object references and signal STONWriterError when a shared reference is encountered
	if #ignore, don''t track object references which might loop forever on cycles
 ';
		immediateInvariant.
true.
%

removeallmethods STONWriter
removeallclassmethods STONWriter

! Class implementation for 'STONReaderError'

!		Class methods for 'STONReaderError'

category: 'instance creation'
classmethod: STONReaderError
signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself
%

!		Instance methods for 'STONReaderError'

category: 'accessing'
method: STONReaderError
buildMessageText
	streamPosition ifNotNil: [ :pos | 
    self details: 'Error at character position ', pos asString 
  ].
  super buildMessageText .
%

category: 'accessing'
method: STONReaderError
streamPosition
	^ streamPosition
%

category: 'accessing'
method: STONReaderError
streamPosition: aNumber
	streamPosition := aNumber
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

category: 'initialization'
classmethod: RwTonelParser
initialize
	self _addInvariantClassVar: #Character_lf value: Character lf
%

category: 'accessing'
classmethod: RwTonelParser
lineEnding
  "Answer the os-specific line endings.  See also #lineEndingSize if changing "

  ^ String with: Character_lf
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
  aString size == 0 ifTrue:[ RwTonelParseError signal:'empty selector string' ].
	^ (aString last = Character space
		ifTrue: [ aString allButLast ]
		ifFalse: [ aString ]) 
		trimLeft
%

category: 'parsing'
method: RwTonelParser
comment
	| result ch eatNext |
	result := String new .
	eatNext := false.
	stream next == $" ifFalse: [ RwTonelParseError signal: 'Can''t parse comment' ].	
	[ stream beforeEnd
		and: [ 
				(ch := stream next) ~~ $" 
				or: [ eatNext := (stream peek == $") ] ] ]
	whileTrue: [ 
		result add: ch.
		eatNext ifTrue: [ 
			stream skip: 1.
			eatNext := false ] ].
	^ self 
		removeFrom: '"',result ,'"' 
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
  | type mlist |
	type := self typeDef.
	mlist := self methodDefList.
	"Optimized  { type . mlist } select: [:each | each ~~ nil ] "
  type ifNotNil:[  mlist ifNotNil:[ ^ { type . mlist } ]
                         ifNil:[ ^ { type } ]].
	mlist ifNotNil:[ ^ { mlist } ].
  ^ { }
%

category: 'error handling'
method: RwTonelParser
error: messageText
	^ RwTonelParseError signal: messageText
%

category: 'private'
method: RwTonelParser
extractSelector: aString
	| separators keywords ofs sz word ch trimmedWord res nKw |
	"separators := { 
		Character space. 
		Character tab. 
		Character lf. 
		Character newPage. 
		Character cr. 
		$: } collect:[:x | x codePoint] "
  separators := #( 32 9 10 12 13 58 ).
	keywords := { } .
  sz := aString size .
  ofs := 0 .
	[ ofs < sz ]
	whileTrue: [  "no temps in this block to avoid complex block"
		word := String new .
		[ofs < sz and: [ (separators includesIdentical: (ch := aString at: (ofs := ofs + 1)) codePoint) == false ] ]
		  whileTrue: [ word add: ch  ].
		ch == $: ifTrue: [ word add: ch ]. 
		trimmedWord := word trimBoth .
    trimmedWord size ~~ 0 ifTrue:[ keywords add: trimmedWord ] 
  ].
	(nKw := keywords size) <= 2 
    ifTrue:[ res := keywords at: 1 ]
    ifFalse:[ res := String new .
             1 to: nKw by: 2 do:[:j | res addAll: (keywords at: j) ]. ].
  ^ res asSymbol .
%

category: 'testing'
method: RwTonelParser
isEnter: aCharacter
  | cp |
  cp := aCharacter codePoint .
  ^ cp == 10 or:[ cp == 13 ]
%

category: 'testing'
method: RwTonelParser
isSeparator: aCharacter 
	"INLINE ^ aCharacter isSeparator"
  ^ aCharacter _unicodeStatus: 29 
%

category: 'accessing'
method: RwTonelParser
lineEndingSize
  "Must be consistent with   self class lineEnding size"
  ^ 1
%

category: 'parsing'
method: RwTonelParser
metadata
	| result ch count |
	stream peek == ${ ifFalse: [ RwTonelParseError signal: 'Can''t parse metadata' ].	
	result := String new .
	count := 0.
	[ stream atEnd ]
	whileFalse: [ 
		ch := stream next.
		result add: ch.
		ch == ${ ifTrue: [ count := count +1 ].
		ch == $} ifTrue: [ count := count -1 ].
		count == 0 ifTrue: [ ^ STON fromString: result ]].

	RwTonelParseError signal: 'Can''t parse metadata'
%

category: 'parsing'
method: RwTonelParser
method
	| type selector |
	
	type := self untilIncluding: '>>'.
	selector := self cleanSelector: (self untilExcludingChar: $[ ).
	type := type trimBoth substringsSpace . "substrings: ' ' "
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
  startPos := stream position .
  "startBody := stream peek: 300 ." "uncomment for debugging parse problems"
	stream peek == $[ ifFalse: [ RwTonelParseError signal: 'Can''t parse method body' ].
	result := String new .
	comment := false.
	string := false.
	prevChar := nil.
	count := 0.

	[ stream atEnd ]
	whileFalse: [ 
		char := stream next.
		result add: char.
		(char == $" and: [ string == false and: [ prevChar ~~ $$ or: [ comment ] ] ]) 
			ifTrue: [ comment := comment ifTrue:[false] ifFalse:[true] ]. 
		(char == $' and: [ comment == "not"false and: [ prevChar ~~ $$ or: [ string ] ] ]) 
			ifTrue: [ string := string "not"ifTrue:[false] ifFalse:[true] ]. 
		(comment or: [ string ]) ifFalse: [ 
			(char == $[ and: [  prevChar ~~ $$ ]) ifTrue: [ count := count +1 ].
			(char == $] and: [ prevChar ~~ $$ ]) ifTrue: [ count := count -1 ] ].
		count == 0 ifTrue: [ 
			^ self 
				removeFrom: result 
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
    nil .  
    nil .
  }.
  offset := stream position . ar at: 4 put: self method .
  ar at: 5 put: self methodBody .

  (def := self newMethodDefinitionFrom: ar )
    offset: offset
    inFile: stream wrappedStreamName .

  aBlock
    value: (((ar at: 4) at: 1) at: 2) ~~ nil
    value: def
%

category: 'parsing'
method: RwTonelParser
methodDefList
	| result classMeths meths |
	self separator. "to arrive to the end of the file in case there are no methods"
  classMeths := { } .
  meths := { } .
	result := { classMeths  . meths  } .
	[
		[ stream atEnd ]
			whileFalse: [ 
				self methodDef: [:isMeta :mDef |
					isMeta
						ifTrue: [ classMeths add:  mDef ]
						ifFalse: [ meths add: mDef ].
					"skip possible spaces at the end"
					self separator ]
			] 
  ] on: Error do:[:ex |
    ((RwTonelParseError,STONReaderError,STONWriterError) handles: ex) ifTrue:[
      lastSelectorParsed ifNotNil:[ | str |
        str := ex details ifNil:[ '' ].
        ex details: str, ', last method parsed: ', lastSelectorParsed printString
      ].
    ].
    ex pass 
  ].
  ^ result
%

category: 'private factory'
method: RwTonelParser
newMethodDefinitionFrom: anArray
	| metadata className meta selector source categ fourth fourthTwo |
	metadata := (anArray at: 2) ifNil: [ Dictionary new ].
	className := ((fourth := anArray at: 4) at: 1) at: 1.
	[ Metaclass3 _validateNewClassName: className asSymbol ]
		on: Error
		do: [ :ex | self error: 'Invalid class name ' , className printString ].
	meta := ((fourth at: 1) at: 2) ~~ nil .
	selector := self extractSelector: (fourthTwo := fourth at: 2) trimBoth.
	source := String new .
  source addAll: fourthTwo .
	(anArray at: 5) ifNotEmpty: [ :src | source addAll: src ].

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
  result peek == startChar
    ifFalse: [ RwTonelParseError signal: 'I cannot remove enclosing start' ].
  result skip: 1.
  (#(#'both' #'left') includesIdentical: cleanSymbol)
    ifTrue: [ 
      stop := self lineEndingSize.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: 1 ] ].
  start := result position.
  result setToEnd.
  result skip: -1.
  result peek == endChar
    ifFalse: [ RwTonelParseError signal: 'I cannot remove enclosing end' ].
  result skip: -1.
  (#(#'both' #'right') includesIdentical: cleanSymbol)
    ifTrue: [ 
      stop := self lineEndingSize.
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
	[ stream beforeEnd and: [ self isSeparator: stream peek ] ]
	whileTrue: [ stream next ].
	^ nil
%

category: 'parsing'
method: RwTonelParser
shebang
	"look for a '#!' in first two character position and skip to next line if present"

	(stream peekFor: $#) ifFalse: [ ^ nil ].	
	(stream peekFor: $!) ifFalse: [ ^ nil ].
	^ stream  upTo: Character_lf.
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
	"^ self 
		try: aBlock 
		onSuccess: [ :parsedValue | parsedValue ] 
		onFailure: [ nil ]"
  | pos |
  pos := stream position. 
  ^ [ aBlock value ] 
    on: RwTonelParseError 
    do:[:ex | stream position: pos . nil ]
%

category: 'private parsing'
method: RwTonelParser
try: aBlock onSuccess: successBlock
	"^ self 
		try: aBlock 
		onSuccess: successBlock 
		onFailure: [ nil ] "
  | pos |
  pos := stream position.
  ^ [ successBlock value: aBlock value ]
    on: RwTonelParseError
    do:[:ex | stream position: pos . nil ]
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
  stream peek ifNotNil:[ :ch |
    ch == $E ifTrue:[ self try: [ self word: 'Extension' ] onSuccess: [ :word | ^ word  ]].
	  ch == $C ifTrue:[ self try: [ self word: 'Class' ] onSuccess: [ :word | ^ word  ]].
   	self try: [ self word: 'Trait' ] onSuccess: [ :word | ^ word  ].
  ].	
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
    stream peek == $" ifTrue:[ self try: [ self comment ]].
		self separator. 
		self type. 
		self separator. 
    stream peek == ${ ifTrue:[
		  self try: [ 
			  | typeMetadata normalizedMetadata |
			  typeMetadata := self metadata.
			  normalizedMetadata := Dictionary new.
			  typeMetadata keysAndValuesDo: [:key :value |
				  normalizedMetadata at: key asLowercase asSymbol put: value ].
			  normalizedMetadata at: #shebang put: shebang.
			  normalizedMetadata ] 
    ].
	}
%

category: 'private parsing'
method: RwTonelParser
untilExcludingChar: aCharacter
	| result |
	result := stream upTo: aCharacter.
	stream position: stream position - 1 .
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

  | warningsEnabled |
  warningsEnabled := Notification signallingEnabled .
  [ 
	  | gsfile stream errBlk warnBlk |
    Notification enableSignalling . "compile warnings can be logged"
	  gsfile := GsFile openReadOnServer: filePath.
	  gsfile ifNil: [ self error: 'file ' , filePath printString , ' not found' ].
	  stream := ReadStreamPortable on: gsfile contents.
	  gsfile close.
		errBlk := [ :ex | 
         ex addText: (self _lineNumberStringForOffset: stream position fileName: filePath).
			   ex pass ].
    warnBlk := [ :warn| | str | 
       str := warn asString .
       ((str subStrings occurrencesOf: 'WARNING:') == 1 
         and:[ str includesString: 'not optimized']) ifFalse:[
         GsFile gciLogServer: warn asString . 
       ].
       warn resume 
    ].
	  [
      self topazReadTonelStream: stream envId: envId 
    ] onException: { STONReaderError . RwTonelParseError . Warning }
		  do: { errBlk . errBlk . warnBlk } .
  ] ensure:[ 
    warningsEnabled ifFalse:[ Notification disableSignalling]
  ]
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

! Class implementation for 'STON'

!		Class methods for 'STON'

category: 'convenience'
classmethod: STON
fromStream: readStream
	^ (self reader on: readStream) next
%

category: 'convenience'
classmethod: STON
fromString: string
  ^ self fromStream: string readStream
%

category: 'accessing'
classmethod: STON
jsonWriter
	^ STONWriter new
		  jsonMode: true;
		  yourself
%

category: 'accessing'
classmethod: STON
listClass
	^ Array
%

category: 'accessing'
classmethod: STON
mapClass
	^ Dictionary
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStream: stream
	(self jsonWriter on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStreamPretty: stream
	(self jsonWriter on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStream: stream
	(self writer on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStreamPretty: stream
	(self writer on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'accessing'
classmethod: STON
reader
	^ STONReader new
%

category: 'convenience'
classmethod: STON
toJsonString: object
  ^ String streamContents: [ :stream | self put: object asJsonOnStream: stream ]
%

category: 'convenience'
classmethod: STON
toJsonStringPretty: object
  ^ String
    streamContents: [ :stream | self put: object asJsonOnStreamPretty: stream ]
%

category: 'convenience'
classmethod: STON
toString: object
  ^ String streamContents: [ :stream | self put: object onStream: stream ]
%

category: 'convenience'
classmethod: STON
toStringPretty: object
  ^ String streamContents: [ :stream | self put: object onStreamPretty: stream ]
%

category: 'accessing'
classmethod: STON
writer
	^ STONWriter new
%

! Class implementation for 'STONReader'

!		Class methods for 'STONReader'

category: 'initialization'
classmethod: STONReader
initialize
	self _addInvariantClassVar: #Character_lf value: Character lf.
	self _addInvariantClassVar: #Character_backspace value: Character backspace.
	self _addInvariantClassVar: #Character_newPage value: Character newPage.
	self _addInvariantClassVar: #Character_cr value: Character cr.
	self _addInvariantClassVar: #Character_tab value: Character tab.
%

category: 'instance creation'
classmethod: STONReader
new
  ^ self basicNew
    initialize;
    yourself
%

category: 'instance creation'
classmethod: STONReader
on: readStream
	^ self new
		on: readStream;
		yourself
%

!		Instance methods for 'STONReader'

category: 'initialize-release'
method: STONReader
allowComplexMapKeys: boolean
	allowComplexMapKeys := boolean
%

category: 'testing'
method: STONReader
atEnd
	^ readStream atEnd
%

category: 'testing'
method: STONReader
beforeEnd
	^ readStream beforeEnd
%

category: 'initialize-release'
method: STONReader
classes

	^ classes
%

category: 'initialize-release'
method: STONReader
close
	readStream ifNotNil: [
		readStream close.
		readStream := nil ]
%

category: 'private'
method: STONReader
consumeWhitespace
	"Strip whitespaces from the input stream."

	[ readStream beforeEnd and: [ readStream peek isSeparator ] ]
		whileTrue: [ readStream next ]
%

category: 'error handling'
method: STONReader
error: aString
	| streamPosition |
	"Remain compatible with streams that don't understand #position"
	streamPosition := [ readStream position ]
		on: MessageNotUnderstood do: [ nil ].
	^ STONReaderError signal: aString streamPosition: streamPosition
%

category: 'private'
method: STONReader
expectChar: character
	"Expect character and consume input and optional whitespace at the end,
	 throw an error otherwise."

	(self matchChar: character)
		ifFalse: [ self error: character asString, ' expected' ]
%

category: 'initialize-release'
method: STONReader
initialize
  objects := IdentityDictionary new.
  classes := IdentityDictionary new.
  allowComplexMapKeys := false.
  stack := OrderedCollection new.
  unresolvedReferences := 0
%

category: 'private'
method: STONReader
isClassChar: aCharacter
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' includesValue: aCharacter
%

category: 'private'
method: STONReader
isClassStartChar: aCharacter
	^ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includesValue: aCharacter
%

category: 'private'
method: STONReader
isSimpleSymbolChar: aCharacter
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includesValue: aCharacter
%

category: 'private'
method: STONReader
lookupClass: name
	^ (System myUserProfile objectNamed: name asSymbol)
		ifNil: [ 
			(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
				ifNotNil: [ :rowanSymbolDictionary | 
					(rowanSymbolDictionary at: name asSymbol ifAbsent: [  ])
						ifNotNil: [ :cls | ^ cls ] ])
				ifNil: [ 
					classes
						at: name
						ifAbsentPut: [ 
							(ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [ 
									(((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan')
										ifNotNil: [ :rowan | rowan platform serviceClassFor: name ])
										ifNil: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ] ]
%

category: 'private'
method: STONReader
match: string do: block
	"Try to read and consume string and execute block if successful.
	Else do nothing (but do not back up)"

	(string allSatisfy: [ :each | readStream peekFor: each ])
		ifTrue: [ 
			self consumeWhitespace.
			block value ]
%

category: 'private'
method: STONReader
matchChar: character
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^ (readStream peekFor: character)
		ifTrue: [ 
			self consumeWhitespace.
			true ]
		ifFalse: [ false ]
%

category: 'private'
method: STONReader
newReference
	| index reference |
	index := objects size + 1.
	reference := STONReference index: index.
	objects at: index put: reference.
	^ reference
%

category: 'public'
method: STONReader
next
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0
		ifTrue: [ self processSubObjectsOf: object ].
	^ object
%

category: 'initialize-release'
method: STONReader
on: aReadStream
	readStream := aReadStream .
%

category: 'private'
method: STONReader
optimizeForLargeStructures
  "nothing special for GemStone"

%

category: 'parsing-internal'
method: STONReader
parseCharacter
  | char |
  (char := readStream next) == $\
    ifFalse: [ ^ char ].
  ( #( $' $" $/ $\ ) includesIdentical: (char := readStream next))
    ifTrue: [ ^ char ].
  char == $b
    ifTrue: [ ^ Character_backspace ].
  char == $f
    ifTrue: [ ^ Character_newPage ].
  char == $n
    ifTrue: [ ^ Character_lf ].
  char == $r
    ifTrue: [ ^ Character_cr ].
  char == $t
    ifTrue: [ ^ Character_tab ].
  char == $u
    ifTrue: [ ^ self parseCharacterHex ].
  self error: 'invalid escape character \' , char .
%

category: 'parsing-internal'
method: STONReader
parseCharacterHex
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value
%

category: 'parsing-internal'
method: STONReader
parseCharacterHexDigit
	| digit |
	readStream atEnd ifFalse: [ 
		digit := readStream next asInteger.
		(digit between: "$0" 48 and: "$9" 57)
			ifTrue: [ ^ digit - 48 ].
		(digit between: "$A" 65 and: "$F" 70)
			ifTrue: [ ^ digit - 55 ].
		(digit between: "$a" 97 and: "$f" 102)
			ifTrue: [ ^ digit - 87 ] ].
	self error: 'hex-digit expected'
%

category: 'parsing-internal'
method: STONReader
parseClass
	| className |
	className := String new .
	[ readStream beforeEnd and: [ self isClassChar: readStream peek ] ] whileTrue: [ 
			className add: readStream next ].
	self consumeWhitespace.
	^ self lookupClass: className asSymbol
%

category: 'parsing-internal'
method: STONReader
parseConstantDo: block
	"Parse and consume either true|false|nil|null and execute block 
	or else do nothing (but do not back up).
	Hand written implementation to avoid the use of #position:"
  | pChar |	
	(pChar := readStream peek) == $t
		ifTrue: [
			^ self match: 'true' do: [ block value: true ] ].
	pChar == $f
		ifTrue: [
			^ self match: 'false' do: [ block value: false ] ].
	pChar == $n
		ifTrue: [
			readStream next.
			(pChar := readStream peek) == $i
				ifTrue: [
					self match: 'il' do: [ block value: nil ] ].
			pChar == $u
				ifTrue: [
					self match: 'ull' do: [ block value: nil ] ] ]
%

category: 'parsing'
method: STONReader
parseList
	| reference array |
	reference := self newReference.
	array := "STON listClass == Array" { } .
	self parseListDo: [ :each | array add: each ] .
	self setReference: reference to: array.
	^ array
%

category: 'parsing'
method: STONReader
parseListDo: block
	| index |
	self expectChar: $[.
	(self matchChar: $]) 
		ifTrue: [ ^ self ].
	index := 1.
	[ readStream atEnd ] whileFalse: [
		block cull: self parseValue cull: index.
		(self matchChar: $]) 
			ifTrue: [ ^ self ].
		index := index + 1.
		self expectChar: $, ].
	self error: 'end of list expected'
%

category: 'parsing'
method: STONReader
parseListSingleton
	| value |
	value := nil.
	self parseListDo: [ :each :index |
		index == 1 ifTrue: [ value := each ] ].
	^ value
%

category: 'parsing'
method: STONReader
parseMap
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [ :key :value |
		map at: key put: value ].
	^ map
%

category: 'parsing'
method: STONReader
parseMapDo: block
  self expectChar: ${.
  (self matchChar: $})
    ifTrue: [ ^ self ].
  [ readStream atEnd ] whileFalse: [ | name value |
      name := self parseValue.
      (allowComplexMapKeys
        or: [ name isString or: [ name isNumber ] ])
        ifFalse: [ self error: 'unexpected property name type' ].
      self expectChar: $:.
      value := self parseValue.
      block value: name value: value.
      (self matchChar: $})
        ifTrue: [ ^ self ].
      self expectChar: $, ].
  self error: 'end of map expected'
%

category: 'parsing-internal'
method: STONReader
parseNumber
	| negated number |
	negated := readStream peekFor: $- .
	number := self parseNumberInteger.
	(readStream peekFor: $. )
		ifTrue: [ number := number + self parseNumberFraction ].
	((readStream peekFor: $e) or: [ readStream peekFor: $E ])
		ifTrue: [ number := number * self parseNumberExponent ].
	negated
		ifTrue: [ number := number negated ].
	self consumeWhitespace.
	^ number
%

category: 'parsing-internal'
method: STONReader
parseNumberExponent
	| number negated |
	number := 0.
	(negated := readStream peekFor: $- )
		ifFalse: [ readStream peekFor: $+ ].
	[ readStream beforeEnd and: [ readStream peek isDigit ] ]
		whileTrue: [ number := 10 * number + readStream next digitValue ].
	negated
		ifTrue: [ number := number negated ].
	^ 10 raisedTo: number
%

category: 'parsing-internal'
method: STONReader
parseNumberFraction
	| number power |
	number := 0.
	power := 1.0.
	[ readStream beforeEnd and: [ readStream peek isDigit ] ] whileTrue: [
		number := 10 * number + readStream next digitValue.
		power := power * 10.0 ].
	^ number / power
%

category: 'parsing-internal'
method: STONReader
parseNumberInteger
	| number |
	number := 0.
	[ readStream beforeEnd and: [ readStream peek isDigit ] ] whileTrue: [ 
		number := 10 * number + readStream next digitValue ].
	^ number
%

category: 'parsing'
method: STONReader
parseObject
	| targetClass reference object |
	targetClass := self parseClass.
	reference := self newReference.
	object := targetClass fromSton: self.
	self setReference: reference to: object.
	^ object
%

category: 'parsing-internal'
method: STONReader
parseReference
	| index |
	self expectChar: $@.
	index := self parseNumberInteger.
	self consumeWhitespace.
	unresolvedReferences := unresolvedReferences + 1.
	^ STONReference index: index
%

category: 'parsing-internal'
method: STONReader
parseString
	^ self parseStringInternal
%

category: 'parsing-internal'
method: STONReader
parseStringInternal
  | result delimiter |
  delimiter := readStream next.
  (delimiter == $' or: [ delimiter == $" ])
    ifFalse: [ self error: ''' or " expected' ].
  result := String new .
   [ readStream atEnd or: [ readStream peek = delimiter ] ]
       whileFalse: [ result add: self parseCharacter ].
  self expectChar: delimiter.
  ^ result
%

category: 'parsing-internal'
method: STONReader
parseSymbol
	| string |
	self expectChar: $#.
	readStream peek == $'
		ifTrue: [ ^ self parseStringInternal asSymbol ].
	string := String new .
	[ readStream beforeEnd and: [ self isSimpleSymbolChar: readStream peek ] ] whileTrue: [
		string add: readStream next ] .
	string size == 0
		ifFalse: [ 
			self consumeWhitespace.
			^ string asSymbol ].
	self error: 'unexpected input'
%

category: 'parsing'
method: STONReader
parseValue
	| char |
	readStream atEnd ifFalse: [ 
		(self isClassStartChar: (char := readStream peek)) 
			ifTrue: [ ^ self parseObject ].
		char == ${
			ifTrue: [ ^ self parseMap ].
		char == $[
			ifTrue: [ ^ self parseList ].
		(char == $' or: [ char == $" ])
			ifTrue: [ ^ self parseString ].
		char == $#
			ifTrue: [ ^ self parseSymbol ].
		char == $@
			ifTrue: [ ^ self parseReference ].
		(char == $- or: [ char isDigit ])
			ifTrue: [ ^ self parseNumber ].
		self parseConstantDo: [ :value | ^ value ] ].
	self error: 'invalid input'
%

category: 'private'
method: STONReader
processSubObjectsOf: object
  stack addFirst: object.
  [ stack isEmpty ]
    whileFalse: [ stack removeFirst stonProcessSubObjects: [ :each | each isStonReference
            ifTrue: [ self resolveReference: each ]
            ifFalse: [ each stonContainSubObjects
                ifTrue: [ stack addFirst: each ]
                ifFalse: [ each ] ] ] ]
%

category: 'initialize-release'
method: STONReader
reset
	unresolvedReferences := 0.
	objects removeAll
%

category: 'private'
method: STONReader
resolveReference: reference
	^ self resolveReferenceIndex: reference index
%

category: 'private'
method: STONReader
resolveReferenceIndex: index
	^ objects at: index
%

category: 'private'
method: STONReader
setReference: reference to: object
	objects at: reference index put: object
%

category: 'private'
method: STONReader
storeReference: object
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^ index
%

! Class implementation for 'STONReference'

!		Class methods for 'STONReference'

category: 'instance creation'
classmethod: STONReference
index: integer
	^ self new
		index: integer;
		yourself
%

!		Instance methods for 'STONReference'

category: 'comparing'
method: STONReference
= anObject
	^ self class == anObject class and: [ self index = anObject index ]
%

category: 'comparing'
method: STONReference
hash
	^ index hash
%

category: 'accessing'
method: STONReference
index
	^ index
%

category: 'accessing'
method: STONReference
index: integer
	index := integer
%

category: 'testing'
method: STONReference
isStonReference
	^ true
%

category: 'printing'
method: STONReference
printOn: stream
	super printOn: stream.
	stream nextPut: $(; print: index; nextPut: $)
%

! Class implementation for 'STONStreamWriter'

!		Class methods for 'STONStreamWriter'

category: 'instance creation'
classmethod: STONStreamWriter
new
  ^ self basicNew
    initialize;
    yourself
%

category: 'instance creation'
classmethod: STONStreamWriter
on: stonWriter
	^ self new
		on: stonWriter;
		yourself
%

!		Instance methods for 'STONStreamWriter'

category: 'initialize-release'
method: STONStreamWriter
initialize
  first := true
%

category: 'initialize-release'
method: STONStreamWriter
on: stonWriter
	writer := stonWriter
%

! Class implementation for 'STONListWriter'

!		Instance methods for 'STONListWriter'

category: 'accessing'
method: STONListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONShortListWriter'

!		Instance methods for 'STONShortListWriter'

category: 'accessing'
method: STONShortListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONMapWriter'

!		Instance methods for 'STONMapWriter'

category: 'accessing'
method: STONMapWriter
at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
%

! Class implementation for 'STONWriter'

!		Class methods for 'STONWriter'

category: 'private'
classmethod: STONWriter
findFirstInString: aString inSet: inclusionMap startingAt: start
  "Trivial, non-primitive version"

  | i stringSize ascii |
  inclusionMap size ~= 256
    ifTrue: [ ^ 0 ].
  i := start.
  stringSize := aString size.
  [ i <= stringSize and: [ ascii := (aString at: i) asciiValue.
      ascii < 256
        ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
        ifFalse: [ true ] ] ] whileTrue: [ i := i + 1 ].
  i > stringSize
    ifTrue: [ ^ 0 ].
  ^ i
%

category: 'class initialization'
classmethod: STONWriter
initialize
	self initializeSTONCharacters.
	self initializeSTONSimpleSymbolCharacters
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONCharacters
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [ :each | 
		STONCharacters at: each + 1 put: #pass ].
	escapes := #( 8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 34 '\"' 39 '\''' 92 '\\' ).
	1 to: escapes size - 1 by: 2 do: [ :index | 
		STONCharacters 
			at: (escapes at: index) + 1
			put: (escapes at: index + 1) ]
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONSimpleSymbolCharacters
  "STONSimpleSymbolCharacters asArray collectWithIndex: [ :each :index |
		each isZero ifTrue: [ (index - 1) asCharacter ] ]."

  STONSimpleSymbolCharacters := (ByteArray new: 256)
    atAllPut: 1;
    yourself.
  1 to: 256 do: [ :each | | char |
    char := (each - 1) asCharacter.
    (self isSimpleSymbolChar: char)
      ifTrue: [ STONSimpleSymbolCharacters at: each put: 0 ] ]
%

category: 'private'
classmethod: STONWriter
isSimpleSymbolChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includesValue: char
%

category: 'instance creation'
classmethod: STONWriter
new
  ^ self basicNew
    initialize;
    yourself
%

category: 'instance creation'
classmethod: STONWriter
on: writeStream
	^ self new
		on: writeStream;
		yourself
%

!		Instance methods for 'STONWriter'

category: 'initialize-release'
method: STONWriter
close
	writeStream ifNotNil: [
		writeStream close.
		writeStream := nil ]
%

category: 'writing'
method: STONWriter
encodeCharacter: char
  | code encoding |
  ((code := char codePoint) < 127
    and: [ (encoding := STONCharacters at: code + 1) notNil ])
    ifTrue: [ encoding = #'pass'
        ifTrue: [ writeStream nextPut: char ]
        ifFalse: [ writeStream nextPutAll: encoding ] ]
    ifFalse: [ | paddedStream padding digits |
      paddedStream := WriteStream on: String new.
      code printOn: paddedStream base: 16 showRadix: false.
      digits := paddedStream contents.
      padding := 4 - digits size.
      writeStream nextPutAll: '\u'.
      encoding := padding > 0
        ifTrue: [ ((String new: padding)
            atAllPut: $0;
            yourself) , digits ]
        ifFalse: [ digits ].
      writeStream nextPutAll: encoding ]
%

category: 'private'
method: STONWriter
encodeKey: key value: value
	self nextPut: key.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value
%

category: 'writing'
method: STONWriter
encodeList: elements
	writeStream nextPut: $[.
	elements isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				elements 
					do: [ :each | self nextPut: each ]
					separatedBy: [ self listElementSeparator ] ].
			self newlineIndent ].
	writeStream nextPut: $]
%

category: 'writing'
method: STONWriter
encodeMap: pairs
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
			self newlineIndent ].
	writeStream nextPut: $}
%

category: 'private'
method: STONWriter
encodeString: string
  | encodedString |
  encodedString := string.
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ]).
  encodedString do: [ :each | self encodeCharacter: each ].
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ])
%

category: 'private'
method: STONWriter
indentedDo: block
	level := level + 1.
	block value.
	level := level - 1
%

category: 'initialize-release'
method: STONWriter
initialize
  prettyPrint := false.
  newLine := String with: Character lf.
  level := 0.
  referencePolicy := #'normal'.
  jsonMode := false.
  objects := IdentityDictionary new
%

category: 'private'
method: STONWriter
isSimpleSymbol: symbol
  symbol isEmpty
    ifTrue: [ ^ false ].
  ^ (self class
    findFirstInString: symbol
    inSet: STONSimpleSymbolCharacters
    startingAt: 1) = 0
%

category: 'initialize-release'
method: STONWriter
jsonMode: boolean
	jsonMode := boolean
%

category: 'private'
method: STONWriter
listElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'private'
method: STONWriter
mapElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'initialize-release'
method: STONWriter
newLine: string
	newLine := string
%

category: 'private'
method: STONWriter
newlineIndent
	prettyPrint ifTrue: [ 
		writeStream nextPutAll: newLine.
		level timesRepeat: [ writeStream tab ] ]
%

category: 'public'
method: STONWriter
nextPut: anObject
	anObject stonOn: self
%

category: 'initialize-release'
method: STONWriter
on: aWriteStream
	writeStream := aWriteStream
%

category: 'private'
method: STONWriter
optimizeForLargeStructures
  "nothing special for GemStone"

%

category: 'initialize-release'
method: STONWriter
prettyPrint: boolean
	prettyPrint := boolean
%

category: 'private'
method: STONWriter
prettyPrintSpace
	prettyPrint ifTrue: [ writeStream space ]
%

category: 'initialize-release'
method: STONWriter
referencePolicy: policy
  (#(#'normal' #'ignore' #'error') includesIdentical: policy)
    ifFalse: [ self error: 'Unknown reference policy: ' , policy printString ].
  referencePolicy := policy
%

category: 'initialize-release'
method: STONWriter
reset
	objects removeAll
%

category: 'private'
method: STONWriter
shortListElementSeparator
	writeStream nextPut: $, .
	self prettyPrintSpace
%

category: 'private'
method: STONWriter
with: object do: block
	| index |
	referencePolicy = #ignore 
		ifTrue: [ ^ block value ].
	(index := objects at: object ifAbsent: [ nil ]) notNil
		ifTrue: [
			referencePolicy = #error
				ifTrue: [ ^ STONWriterError signal: 'Shared reference detected' ].
			self writeReference: index ]
		ifFalse: [
			index := objects size + 1.
			objects at: object put: index.
			block value ]
%

category: 'writing'
method: STONWriter
writeBoolean: boolean
	writeStream print: boolean
%

category: 'writing'
method: STONWriter
writeFloat: float
  writeStream nextPutAll: float asString
%

category: 'writing'
method: STONWriter
writeInteger: integer
	writeStream print: integer
%

category: 'writing'
method: STONWriter
writeList: collection
	self with: collection do: [ 
		self encodeList: collection ]
%

category: 'writing'
method: STONWriter
writeMap: hashedCollection
	self with: hashedCollection do: [ 
		self encodeMap: hashedCollection ]
%

category: 'writing'
method: STONWriter
writeNull
	jsonMode
		ifTrue: [ writeStream nextPutAll: 'null' ]
		ifFalse: [ writeStream print: nil ]
%

category: 'writing'
method: STONWriter
writeObject: anObject
  | instanceVariableNames |
  (instanceVariableNames := anObject class allInstVarNames) isEmpty
    ifTrue: [ self writeObject: anObject do: [ self encodeMap: #() ] ]
    ifFalse: [ self writeObject: anObject streamMap: [ :dictionary | instanceVariableNames
            do: [ :each | (anObject instVarAt: (instanceVariableNames indexOf: each asSymbol))
                ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
                ifNil: [ anObject stonShouldWriteNilInstVars
                    ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ] ]
%

category: 'writing'
method: STONWriter
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ STONWriterError signal: 'Wrong object class for JSON mode' ].
	self with: anObject do: [
		writeStream nextPutAll: anObject class stonName.
		self prettyPrintSpace.
		block value ]
%

category: 'writing'
method: STONWriter
writeObject: object listSingleton: element
	self writeObject: object do: [
		writeStream nextPut: $[.
		self 
			prettyPrintSpace;
			nextPut: element;
			prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self newlineIndent.
			block value: listWriter ].
		self newlineIndent.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamMap: block
	self writeObject: object do: [ | mapWriter |
		mapWriter := STONMapWriter on: self.
		writeStream nextPut: ${.
		self indentedDo: [
			self newlineIndent.
			block value: mapWriter ].
		self newlineIndent.
		writeStream nextPut: $} ]
%

category: 'writing'
method: STONWriter
writeObject: object streamShortList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONShortListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self prettyPrintSpace.
			block value: listWriter ].
		self prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeReference: index
	writeStream
		nextPut: $@;
		print: index
%

category: 'writing'
method: STONWriter
writeString: string
	self encodeString: string
%

category: 'writing'
method: STONWriter
writeSymbol: symbol
	jsonMode
		ifTrue: [
			self writeString: symbol ]
		ifFalse: [
			writeStream nextPut: $#.
			(self isSimpleSymbol: symbol)
				ifTrue: [
					writeStream nextPutAll: symbol ]
				ifFalse: [
					self encodeString: symbol ] ]
%

! Class extensions for 'AbstractDictionary'

!		Class methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
classmethod: AbstractDictionary
fromSton: stonReader
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
%

!		Instance methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]
%

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
	(self class isVariable and: [ self class isBytes not and: [self class isIndexable]])
		ifTrue: [
			1 to: self _basicSize do: [ :each | |val|			
									val:= (block value: (self basicAt: each)).
									self basicAt: each put: val ] ]"
							super stonProcessSubObjects: block"
%

! Class extensions for 'Boolean'

!		Instance methods for 'Boolean'

category: '*ston-gemstone-kernel'
method: Boolean
stonContainSubObjects 
	^ false
%

category: '*ston-gemstone-kernel'
method: Boolean
stonOn: stonWriter
	stonWriter writeBoolean: self
%

! Class extensions for 'ByteArray'

!		Class methods for 'ByteArray'

category: '*ston-gemstone-kernel'
classmethod: ByteArray
fromSton: stonReader
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream
%

!		Instance methods for 'ByteArray'

category: '*ston-gemstonebase'
method: ByteArray
readHexFrom: aStream
  "Initialize the receiver from a hexadecimal string representation"

  | map v ch value |
  map := '0123456789abcdefABCDEF'.
  1 to: self size do: [ :i | 
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := v bitShift: 4.
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := value + v.
    self at: i put: value ]
%

category: '*ston-gemstone-kernel'
method: ByteArray
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: ByteArray
stonOn: stonWriter
  "Use a hex representation"

  stonWriter writeObject: self listSingleton: self asHexString
%

! Class extensions for 'Character'

!		Class methods for 'Character'

category: '*ston-gemstone-kernel'
classmethod: Character
fromSton: stonReader
	^ stonReader parseListSingleton first
%

!		Instance methods for 'Character'

category: '*rowan-tonel-gemstone-kernel'
method: Character
isCharacter

	^ true
%

category: '*ston-gemstone-kernel'
method: Character
stonOn: stonWriter
	stonWriter writeObject: self listSingleton: self asString
%

! Class extensions for 'CharacterCollection'

!		Class methods for 'CharacterCollection'

category: '*ston-gemstonecommon'
classmethod: CharacterCollection
findFirstInString: aString inSet: inclusionMap startingAt: start

	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256
		ifTrue: [ ^ 0 ].

	i := start.
	stringSize := aString size.
	[ 
	i <= stringSize
		and: [ 
			ascii := (aString at: i) asciiValue.
			ascii < 256
				ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
				ifFalse: [ true ] ] ]
		whileTrue: [ i := i + 1 ].

	i > stringSize
		ifTrue: [ ^ 0 ].
	^ i
%

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

category: '*ston-gemstonebase'
method: CharacterCollection
isString
  ^ true
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
  | res |
  res := self class new .
  aCollection do:[:each | res addAll: each asString ] 
			 separatedBy:[ res addAll: self ] .
  ^ res
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
		(nextLF == 0 and: [ nextCR == 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR == 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR == nextLF
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

category: '*ston-gemstonecommon'
method: CharacterCollection
stonContainSubObjects
  ^ false
%

category: '*ston-gemstonecommon'
method: CharacterCollection
stonOn: stonWriter

        self isSymbol
                ifTrue: [stonWriter writeSymbol: self]
                ifFalse: [stonWriter writeString: self]
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result subString ofs sz |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	ofs := 1 .
  sz := self size .
	result := { } .
	subString := String new .
	[ ofs > sz ] whileFalse: [
		| char |
		char := self at: ofs . ofs := ofs + 1 . 
		(separators includesValue: char)
			ifTrue: [
				subString size == 0 ifFalse: [
					result add: subString .
					subString := String new ] ]
			ifFalse: [
				subString add: char ] ].
	subString size == 0 ifFalse: [ result add: subString ].
	^ result 
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
substringsSpace
	"Answer an array containing the substrings in the receiver separated 
	by  Character space"
	| result ofs subStr sepChar sz |
  sepChar := $  "Character space".
	ofs := 1 .
  sz := self size .
	result := { } .
	subStr := String new .
	[ ofs > sz ] whileFalse: [
		| char |
		char := self at: ofs . ofs := ofs + 1 .
		char == sepChar 
			ifTrue: [
				subStr size == 0 ifFalse: [
					result add: subStr .
					subStr := String new ] ]
			ifFalse: [
				subStr add: char ] ].
	subStr size == 0 ifFalse: [ result add: subStr ].
	^ result 
%

category: '*rowan-tonel-gemstone-kernel'
method: CharacterCollection
trimBoth

	"Trim separators from both sides of the receiving string."
  | left right |
  left := 1 .
  right := self size .
  [ left <= right and:[ (self at: left) isSeparator ]] whileTrue:[ left := left + 1].
  left > right ifTrue:[ ^ self class new ].
  [ (self at: right) isSeparator ] whileTrue:[ right := right - 1].
	^ self copyFrom: left to: right .
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
  | left right |
  left := 1 .
  right := self size .
  [ left <= right and:[ (self at: left) isSeparator ]] whileTrue:[ left := left + 1].
  left > right ifTrue:[ ^ self class new ].
	^ self copyFrom: left to: right .
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
  | right |
  right := self size .
  [ right >= 1 and:[ (self at: right) isSeparator ]] whileTrue:[ right := right - 1].
  right == 0 ifTrue:[ ^ self class new ].
	^ self copyFrom: 1 to: right .
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

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: '*ston-gemstone-kernel'
method: Class
stonName
	"Override to encode my instances using a different class name."
	
	^ self name
%

! Class extensions for 'Collection'

!		Class methods for 'Collection'

category: '*ston-gemstone-kernel'
classmethod: Collection
fromSton: stonReader
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
%

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
	
	"^ Array streamContents: [ :stream | self flattenOn: stream]."
  | a |
  a := Array new .
  self _flattendAppend: a .
  ^ a
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

category: '*rowan-tonel-gemstone-kernel'
method: Collection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
sort: aSortBlock

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock
%

category: '*ston-gemstone-kernel'
method: Collection
stonOn: stonWriter
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]
%

category: '*rowan-tonel-gemstone-kernel'
method: Collection
_flattendAppend: anArray

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each _flattendAppend: anArray]
						ifFalse: [anArray add: each]].
%

! Class extensions for 'CollisionBucket'

!		Instance methods for 'CollisionBucket'

category: '*ston-gemstonecommon'
method: CollisionBucket
stonContainSubObjects 
	^false
%

! Class extensions for 'Date'

!		Class methods for 'Date'

category: '*ston-gemstonecommon'
classmethod: Date
fromSton: stonReader

	^ self fromStream: stonReader parseListSingleton readStream usingFormat: #(3 2 1 $- 1 1)
%

!		Instance methods for 'Date'

category: '*ston-gemstone-kernel'
method: Date
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Date
stonOn: stonWriter
  "Use an ISO style YYYYMMDD representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #(3 2 1 $- 1 1 $: false))
%

! Class extensions for 'DateAndTime'

!		Class methods for 'DateAndTime'

category: '*ston-gemstone-kernel'
classmethod: DateAndTime
fromSton: stonReader
  ^ DateAndTime fromString: stonReader parseListSingleton
%

!		Instance methods for 'DateAndTime'

category: '*ston-gemstone-kernel'
method: DateAndTime
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: DateAndTime
stonOn: stonWriter
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: self printString 
%

! Class extensions for 'GsFile'

!		Instance methods for 'GsFile'

category: '*rowan-tonel-gemstone-kernel'
method: GsFile
<< items

 	items putOn: self.
	
	^ self
%

category: '*rowan-tonel-gemstone-kernel'
method: GsFile
wrappedStreamName
	^ self pathName
%

! Class extensions for 'Integer'

!		Instance methods for 'Integer'

category: '*ston-gemstone-kernel'
method: Integer
stonOn: stonWriter
	stonWriter writeInteger: self
%

! Class extensions for 'Number'

!		Instance methods for 'Number'

category: '*ston-gemstone-kernel'
method: Number
stonContainSubObjects 
	^ false
%

category: '*ston-gemstone-kernel'
method: Number
stonOn: stonWriter
	stonWriter writeFloat: self asFloat
%

! Class extensions for 'Object'

!		Class methods for 'Object'

category: '*ston-gemstone-kernel'
classmethod: Object
fromSton: stonReader
	"Create a new instance and delegate decoding to instance side.
	Override only when new instance should be created directly (see implementors). "
	
	^ self new
		fromSton: stonReader;
		yourself
%

!		Instance methods for 'Object'

category: '*ston-gemstone-kernel'
method: Object
fromSton: stonReader
  "Decode non-variable classes from a map of their instance variables and values.
	Override to customize and add a matching #toSton: (see implementors)."

  self class isVariable
    ifTrue: [ self subclassResponsibility ]
    ifFalse: [ | instanceVariableNames |
      instanceVariableNames := self class allInstVarNames.
      stonReader
        parseMapDo: [ :instVarName :value | self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ]
%

category: '*rowan-tonel-gemstone-kernel'
method: Object
isCharacter

	^ false
%

category: '*rowan-tonel-gemstone-kernel'
method: Object
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false
%

category: '*ston-gemstonebase'
method: Object
isNumber
  ^ self _isNumber
%

category: '*ston-gemstone-kernel'
method: Object
isStonReference
	^ false
%

category: '*ston-gemstonebase'
method: Object
isString
  ^ false
%

category: '*rowan-tonel-gemstone-kernel'
method: Object
putOn: aStream

	^ aStream nextPut: self
%

category: '*ston-gemstone-kernel'
method: Object
stonContainSubObjects
	"Return true if I contain subObjects that should be processed, false otherwise.
	Overwrite when necessary. See also #stonProcessSubObjects:"
	
	^ true
%

category: '*ston-gemstone-kernel'
method: Object
stonOn: stonWriter
	"Encode non-variable classes with a map of their instance variable and values.
	Override to customize and add a matching #fromSton: (see implementors)."

	self class isVariable 
		ifTrue: [
			self subclassResponsibility ]
		ifFalse: [
			stonWriter writeObject: self ]
%

category: '*ston-gemstonecommon'
method: Object
stonProcessSubObjects: block
  "Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."

  1 to: self class instSize do: [ :each | self instVarAt: each put: (block value: (self instVarAt: each)) ].
  (self class isVariable and: [ self class isBytes not ])
    ifTrue: [ 1 to: self _basicSize do: [ :each | self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

category: '*ston-gemstone-kernel'
method: Object
stonShouldWriteNilInstVars
	"Return true if my instance variables that are nil should be written out, 
	false otherwise. Overwrite when necessary. By default, return false."
	
	^ false
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

!		Class methods for 'SequenceableCollection'

category: '*ston-gemstone-kernel'
classmethod: SequenceableCollection
fromSton: stonReader
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
new: newSize streamContents: blockWithArg
  | stream |
  stream := WriteStreamPortable on: (self new: newSize).
  blockWithArg value: stream.
  ^ stream contents
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
streamContents: blockWithArg
  ^ self new: 100 streamContents: blockWithArg
%

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

category: '*ston-gemstone-kernel'
method: SequenceableCollection
stonOn: stonWriter
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
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

category: '*rowan-tonel-gemstone-kernel'
method: SequenceableCollection
writeStreamPortable

	^ WriteStreamPortable on: self
%

! Class extensions for 'SmallDate'

!		Class methods for 'SmallDate'

category: '*ston-gemstone-kernel36x'
classmethod: SmallDate
stonName
	"Need to use a well-known class name. Instances of Date converted to SmallDate if in range"
	
	^ 'Date'
%

! Class extensions for 'SmallDateAndTime'

!		Class methods for 'SmallDateAndTime'

category: '*ston-gemstone-kernel36x'
classmethod: SmallDateAndTime
stonName
	"Need to use a well-known class name. Instances of DateAndTime converted to SmallDateAndTime if in range"
	
	^ 'DateAndTime'
%

! Class extensions for 'SmallTime'

!		Class methods for 'SmallTime'

category: '*ston-gemstone-kernel36x'
classmethod: SmallTime
stonName
	"Need to use a well-known class name. Instances of Time converted to SmallTime if in range"
	
	^ 'Time'
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*rowan-tonel-gemstone-kernel'
method: Stream
<< items

	items putOn: self
%

category: '*rowan-tonel-gemstone-kernel'
method: Stream
wrappedStreamName

	^''
%

! Class extensions for 'String'

!		Instance methods for 'String'

category: '*ston-gemstone-kernel'
method: String
stonContainSubObjects 
	^ false
%

category: '*ston-gemstone-kernel'
method: String
stonOn: stonWriter
	stonWriter writeString: self
%

! Class extensions for 'Symbol'

!		Instance methods for 'Symbol'

category: '*ston-gemstone-kernel'
method: Symbol
stonOn: stonWriter
	stonWriter writeSymbol: self
%

! Class extensions for 'Time'

!		Class methods for 'Time'

category: '*ston-gemstonecommon'
classmethod: Time
fromSton: stonReader
  ^ self fromString: stonReader parseListSingleton usingFormat: #($: true false)
%

!		Instance methods for 'Time'

category: '*ston-gemstone-kernel'
method: Time
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Time
stonOn: stonWriter
  "Use an ISO style HH:MM:SS representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #($: true false))
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

category: '*ston-gemstone-kernel'
method: UndefinedObject
stonContainSubObjects 
	^ false
%

category: '*ston-gemstone-kernel'
method: UndefinedObject
stonOn: stonWriter
	stonWriter writeNull
%

! Class extensions for 'UnorderedCollection'

!		Instance methods for 'UnorderedCollection'

category: '*ston-gemstonecommon'
method: UnorderedCollection
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
"increase the starting index by 4 because of the private inst vars in UnorderedCollection"

	5 to: self class instSize do: [ :each |
		self instVarAt: each  put: (block value: (self instVarAt: each)) ].
	(self class isVariable and: [ self class isBytes not ])
		ifTrue: [
			1 to: self _basicSize do: [ :each |
				self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

! Class Initialization

run
RwTonelParser initialize.
STONReader initialize.
STONWriter initialize.
true
%
