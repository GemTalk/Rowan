"
Support class for topaz `tfile` and `tmethod` commands.

`tmethod`
	Read and compile a single tonal format method from a given string

`tfile`
	Read a single tonel format class from a file and compile the methods within that file. Definition/redefinition of the class not implemented yet.
"
Class {
	#name : 'RwTopazTonelReader',
	#superclass : 'Object',
	#instVars : [
		'environmentId',
		'theClassName'
	],
	#category : 'Rowan-Tonel-Core'
}

{ #category : 'private' }
RwTopazTonelReader class >> _lineNumberStringForOffset: offset fileName: fName [
	| res |
	res := '  (Unable to determine line number)'.
	[ 
	| buf lNum |
	buf := fName asFileReference contents.
	buf size > offset
		ifTrue: [ buf size: offset ].
	lNum := 1 + (buf occurrencesOf: Character lf).
	res := ' near line ' , lNum asString , ' in file ' , fName ]
		on: Error
		do: [ :ex | 
			"ignore"
			 ].
	^ res
]

{ #category : 'instance creation' }
RwTopazTonelReader class >> forEnvironmentId: environmentId [
	"Create a new instance of the receiver that will compile methods using environmentId"

	^ self new
		environmentId: environmentId;
		yourself
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazCompileTonelMethod: aString [
	"Read and compile a single tonal format method from a given string.
		For topaz TMETHOD command"

	^ self topazCompileTonelMethod: aString envId: 0
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazCompileTonelMethod: aString envId: envId [
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
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazReadTonelFile: filePath [
	"Read a single tonel format class from a file and compile the methods within that file. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	^ self topazReadTonelFile: filePath envId: 0
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazReadTonelFile: filePath envId: envId [
	"Read a single tonel format class from a file and compile the methods within that file. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	| warningsEnabled |
	warningsEnabled := Notification signallingEnabled.
	[ 
	| gsfile stream errBlk warnBlk |
	Notification enableSignalling.	"compile warnings can be logged"
	gsfile := GsFile openReadOnServer: filePath.
	gsfile ifNil: [ self error: 'file ' , filePath printString , ' not found' ].
	stream := ReadStreamPortable on: gsfile contents.
	gsfile close.
	errBlk := [ :ex | 
	(ex isKindOf: CompileError)
		ifTrue: [ 
			GsFile
				gciLogServer:
					'CompileError encountered '
						, (self _lineNumberStringForOffset: stream position fileName: filePath) ]
		ifFalse: [ 
			ex
				addText:
					(self _lineNumberStringForOffset: stream position fileName: filePath) ].
	ex pass ].
	warnBlk := [ :warn | 
	| str |
	str := warn asString.
	((str subStrings occurrencesOf: 'WARNING:') == 1
		and: [ str includesString: 'not optimized' ])
		ifFalse: [ GsFile gciLogServer: warn asString ].
	warn resume ].
	[ self topazReadTonelStream: stream envId: envId ]
		onException:
			{STONReaderError.
			RwTonelParseError.
			Error.
			Warning}
		do:
			{errBlk.
			errBlk.
			errBlk.
			warnBlk} ]
		ensure: [ 
			warningsEnabled
				ifFalse: [ Notification disableSignalling ] ]
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazReadTonelStream: tonelStream envId: envId [
	"Used in implementation of the topaz TFILE command.
   Read a single tonel format class from a stream and compile the methods on that stream. 
   It file is  a class.st file, 
   creation of the class is attempted by RwTopazTonelReader >> createClassFrom:"

	RwTonelParser
		parseStream: tonelStream
		forReader: (self forEnvironmentId: envId)
]

{ #category : 'tonel parser interface' }
RwTopazTonelReader >> createClassFrom: anArray [
  "Execute class creation.  
   The typical use cases are filein of tonel files from tests/ernie , 
   or slowfilein or patching of a base image using topaz.
   Signal an error if the creation creates a new version of the class .
   If the class is found in the symbolList , 
   argument to inDictionary: is the dictionary in which it was found. 
   If class not found in the symbolList and we are SystemUser and not in a solo session,
   the arg to inDictionary: will be Globals, 
   else the arg will be UserGlobals of the current session."

  | dict instVars classVars superN name categ knownKeys xKeys sess intoDict 
    oldClass theClass  options type keys superCls classInstVars comment resOop | 
  dict := anArray at: 6 .
  knownKeys := IdentitySet withAll:#( name superclass instvars classvars classinstvars 
                                           category gs_options type gs_reservedoop ).
  keys := IdentitySet new .
  dict keys do:[:k|  keys add: k asSymbol ].
  (xKeys := keys - knownKeys) size > 0 
    ifTrue:[ Error signal:'unknown keys ', (Array withAll: xKeys) printString ].
  sess := GsCurrentSession currentSession .
  (superCls := (sess resolveSymbol: (superN := (dict at: #superclass ) asSymbol))) 
     ifNil:[ Error signal: superN , ' not found' ].
  superCls := superCls _value .
  superCls ifNil:[ Error signal: superN , ' not defined yet' ].  
  name := dict at: #name .
  self theClassName: name .
  categ := dict at: #category otherwise: nil .
  instVars := dict at: #instvars otherwise: #() .
  classVars := dict at: #classvars otherwise: #() .
  classInstVars := dict at: #classinstvars otherwise: #() .
  options := dict at: #gs_options otherwise: #() .
  1 to: options size do:[:j | | opt |
    (opt := Symbol _existingWithAll:( options at: j)) ifNil:[
      Error signal:'invalid option ', opt printString .
    ].
    options at: j put: opt.
  ].
  comment := anArray at: 2 .
  type := dict at: #type otherwise: nil .
  (sess symbolList dictionariesAndAssociationsOf: name asSymbol) ifNotNil:[:ary | | pair |
    ary size > 1 ifTrue:[ Error signal:'More than one definition in symbolList for ', name ].
    pair := ary at: 1 .
    oldClass := (pair at: 2) _value  .
    intoDict := (pair at: 1) 
  ].
  intoDict ifNil:[ 
    intoDict := ((System myUserProfile userId = 'SystemUser') and:[ sess isSolo not]) 
               ifTrue:[ Globals ] 
              ifFalse:[ (sess resolveSymbol: #UserGlobals) _value ].
  ].
  resOop := dict at: #gs_reservedoop otherwise: nil .
  theClass := superCls .
  type ifNil:[
    resOop ifNotNil:[
      theClass := superCls _newKernelSubclass: name 
         instVarNames: instVars classVars: classVars classInstVars: classInstVars
          poolDictionaries:#() inDictionary: intoDict options: options reservedOop: resOop
    ] ifNil:[
      theClass := superCls subclass: name 
         instVarNames: instVars classVars: classVars classInstVars: classInstVars
          poolDictionaries:#() inDictionary: intoDict options: options .
    ]
  ] ifNotNil:[
    type = 'variable' ifTrue:[
      resOop ifNotNil:[
        theClass := superCls _newKernelIndexableSubclass: name 
         instVarNames: instVars classVars: classVars classInstVars: classInstVars
          poolDictionaries:#() inDictionary: intoDict options: options reservedOop: resOop
      ] ifNil:[
        theClass := superCls indexableSubclass: name 
         instVarNames: instVars classVars: classVars classInstVars: classInstVars
          poolDictionaries:#() inDictionary: intoDict options: options .
      ]
    ].
    type = 'byteSubclass' ifTrue:[
      instVars size ~~ 0 ifTrue:[ Error signal:'instVars not allowed on a byte subclass'].
      resOop ifNotNil:[
        classInstVars size ~~ 0 ifTrue:[ Error signal:' classInstVars not allowed'].
        theClass := superCls _newKernelByteSubclass: name 
          classVars: classVars poolDictionaries:#() inDictionary: intoDict 
             options: options reservedOop: resOop
      ] ifNil:[
        theClass := superCls byteSubclass: name 
         classVars: classVars classInstVars: classInstVars
          poolDictionaries:#() inDictionary: intoDict options: options .
      ].
    ].
    theClass ifNil:[ Error signal:'unsupported type ', type printString ].
  ].
  (oldClass ~~ nil and:[ oldClass ~~ theClass]) ifTrue:[ 
    GsFile gciLogServer:'--- old class: ' , oldClass definition .
    GsFile gciLogServer:'--- new class: ' , theClass definition .
    Error signal:'new version of ', name, ' created'.
  ].
  comment ifNotNil:[ theClass comment: comment ].
]

{ #category : 'accessing' }
RwTopazTonelReader >> environmentId [
	^ environmentId ifNil: [ environmentId := 0 ]
]

{ #category : 'accessing' }
RwTopazTonelReader >> environmentId: object [
	environmentId := object
]

{ #category : 'tonel parser interface' }
RwTopazTonelReader >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
	"for topaz TFILE and TMETHOD commands, compile the method instead of creating a method definition"

	| behavior symbolList |
	symbolList := GsCurrentSession currentSession symbolList.
	(self theClassName notNil and: [ className ~= self theClassName ]) "tmethod command uses className to identify the target class" 
		ifTrue: [ 
			self
				error:
					'The name of the class (', className printString, ') specified for the method ' , selector printString
						, ' does not match the name of class (', self theClassName printString, ') declared in this tonel class file' ].
	behavior := symbolList objectNamed: className asSymbol.
	meta
		ifTrue: [ behavior := behavior class ].
	behavior
		compileMethod: source
		dictionaries: symbolList
		category: category
		environmentId: self environmentId
]

{ #category : 'tonel parser interface' }
RwTopazTonelReader >> newTypeDefinitionFrom: anArray [
  | knd |
  (knd := anArray at: 4) = 'Class' ifTrue:[
    self createClassFrom: anArray
  ] ifFalse:[
    knd = 'Extension' ifTrue:[ | name dict |
      dict := anArray at: 6 .
      name := dict at: #name .
      theClassName := name .
    ] ifFalse:[ Error signal: 'uknown tonel file kind ', knd printString ]
  ].
]

{ #category : 'method definition' }
RwTopazTonelReader >> offset: anInteger inFile: aFileName [
	"message sent to method definitions ... avoid MNU"
]

{ #category : 'accessing' }
RwTopazTonelReader >> theClassName [
	^theClassName
]

{ #category : 'accessing' }
RwTopazTonelReader >> theClassName: object [
	theClassName := object
]
