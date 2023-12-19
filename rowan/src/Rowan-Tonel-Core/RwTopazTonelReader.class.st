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
		'environmentId'
	],
	#category : 'Rowan-Tonel-Core'
}

{ #category : 'private' }
RwTopazTonelReader class >> _lineNumberStringForOffset: offset fileName: fName [
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
    ] onException: { STONReaderError . RwTonelParseError . Error . Warning }
		  do: { errBlk . errBlk . errBlk . warnBlk } .
  ] ensure:[ 
    warningsEnabled ifFalse:[ Notification disableSignalling]
  ]
]

{ #category : 'topaz support' }
RwTopazTonelReader class >> topazReadTonelStream: tonelStream envId: envId [
	"Read a single tonel format class from a stream and compile the methods on that stream. 
		Definition/redefinition of the class not implemented yet.
		For topaz TFILE command"

	RwTonelParser
		parseStream: tonelStream
		forReader: (self forEnvironmentId: envId)
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
	"class definition/redefinition not supported"
]

{ #category : 'method definition' }
RwTopazTonelReader >> offset: anInteger inFile: aFileName [
	"message sent to method definitions ... avoid MNU"
]
