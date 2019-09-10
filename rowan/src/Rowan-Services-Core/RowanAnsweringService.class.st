"
A place to put miscellaneous commands that don't fit 
well in other services. 

Also good asking the server questions as it gives back 
an answer whereas other services simply return updated
services.
"
Class {
	#name : 'RowanAnsweringService',
	#superclass : 'RowanService',
	#instVars : [
		'answer'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanAnsweringService >> allTestsIn: classServices [
	answer := Array new. 
	classServices do:[:service | answer addAll: service allTests].
	RowanCommandResult addResult: self.
]

{ #category : 'other' }
RowanAnsweringService >> answer [

	^answer
]

{ #category : 'Updating' }
RowanAnsweringService >> answer: anObject [

	answer := anObject
]

{ #category : 'client commands' }
RowanAnsweringService >> autoCommit [

	answer := RowanService autoCommit. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> breakPointsAreDisabled [
  answer := RowanService breakPointsAreDisabled.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
]

{ #category : 'client commands' }
RowanAnsweringService >> clearAllBreaks [
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod clearAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
]

{ #category : 'client commands' }
RowanAnsweringService >> clearMethodBreaks: methodServices [
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        clearMethodBreaks ]
]

{ #category : 'client commands' }
RowanAnsweringService >> disableAllBreaks [
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod _disableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
]

{ #category : 'client commands' }
RowanAnsweringService >> enableAllBreaks [
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod _enableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString [

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString context: oop [

	answer := [true -> (aString evaluateInContext: (Object _objectForOop: oop) symbolList: Rowan image symbolList) asOop] 
		on: CompileError do: [:ex | 
			false -> ex errorDetails
	].
	answer key ifTrue:[(RowanService autoCommit == true) ifTrue:[System commitTransaction]].
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString inFrame: level ofProcess: processOop context: oop [
  | symbolList frameContents symbolDictionary process |
  symbolList := Rowan image symbolList.
  process := Object _objectForOop: processOop.
  frameContents := process _frameContentsAt: 4.
  symbolDictionary := SymbolDictionary new.
  1 to: (frameContents at: 9) size do: [ :index | 
    ((frameContents at: 9) at: index) first = $.
      ifFalse: [ 
        symbolDictionary
          at: ((frameContents at: 9) at: index) asSymbol
          put: (frameContents at: 11 + index - 1) ] ].
  symbolList add: symbolDictionary before: symbolList first.
  [ 
  answer := [ 
  true
    ->
      (aString evaluateInContext: (Object _objectForOop: oop) symbolList: symbolList)
        asOop ]
    on: CompileError
    do: [ :ex | false -> ex errorDetails ].
  answer key
    ifTrue: [ 
      RowanService autoCommit == true
        ifTrue: [ System commitTransaction ] ].
  RowanCommandResult addResult: self ]
    ensure: [ 
      1 to: (frameContents at: 9) size do: [ :index | 
        | argsAndTemps |
        argsAndTemps := frameContents at: 9.
        (argsAndTemps at: index) first = $.
          ifFalse: [ 
            | variableService |
            process
              _frameAt: level
              tempAt: index
              put: (symbolDictionary at: (argsAndTemps at: index)). 
            variableService := RowanVariableService
              oop: (symbolDictionary at: (argsAndTemps at: index)) asOop
              key: (argsAndTemps at: index)
              value: (symbolDictionary at: (argsAndTemps at: index))
              className: (frameContents at: 8) class name asString.
            RowanCommandResult addResult: variableService ] ].
      symbolList remove: symbolDictionary ].	"return answer for testing"
  ^ answer
]

{ #category : 'client commands' }
RowanAnsweringService >> flipTranscript [
	self isTranscriptInstalled ifTrue:[
		self jadeiteServer uninstallTranscript]
	ifFalse:[
		self jadeiteServer installTranscript]
]

{ #category : 'client commands' }
RowanAnsweringService >> initializeAutoCommit [

	RowanService setAutoCommit: false
]

{ #category : 'client commands' }
RowanAnsweringService >> initializeBreakPointsAreDisabled [
  RowanService setBreakPointsAreDisabled: false
]

{ #category : 'client commands' }
RowanAnsweringService >> interactionHandlerActive [
  answer := SessionTemps current
    at: #'rowanServiceInteractionActive'
    ifAbsent: [ true ].
  RowanCommandResult addResult: self
]

{ #category : 'testing' }
RowanAnsweringService >> isTranscriptInstalled [

	^self transcriptObject == self jadeiteServer
]

{ #category : 'client commands' }
RowanAnsweringService >> loadedPackageExists: packageName [
	
	| actualName |
	actualName := Rowan image packageNames detect:[:loadedName | loadedName asLowercase = packageName asLowercase] ifNone:[]. 
	answer := (Rowan image loadedPackageNamed: actualName ifAbsent: []) notNil. 
	command := nil. 
	commandArgs := nil. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> needsCommit [

	answer := System needsCommit.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> printStringOf: oop toMaxSize: integer [

	answer := (Object _objectForOop: oop) printString. 
	answer := answer size > integer ifTrue: [(answer copyFrom: 1 to: integer) , '...'] ifFalse:[answer].
	RowanCommandResult addResult: self.
	^answer
]

{ #category : 'client commands' }
RowanAnsweringService >> runMethodTests: methodServices [

	| behavior |
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior := methodService classFromName. 
			behavior debug: methodService selector]].
	answer := true. 
	RowanCommandResult initializeResults. "squash any client updates during server test run"
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> selectorsMatchingPattern: pattern [

	answer := ((AllUsers userWithId: #SymbolUser) resolveSymbol: #AllSymbols) value.
	answer := answer select: [:each |each _matchPatternNoCase: pattern].
	answer := answer asSortedCollection asArray.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> setAutoCommit: object [

	answer := RowanService setAutoCommit: object.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> setBreakPointsAreDisabled: boolean [
  boolean
    ifTrue: [ self disableAllBreaks ]
    ifFalse: [ self enableAllBreaks ].
  RowanService setBreakPointsAreDisabled: boolean
]

{ #category : 'client commands' }
RowanAnsweringService >> setEnableInteractionHandler: boolean [
  SessionTemps current at: #'rowanServiceInteractionActive' put: boolean
]

{ #category : 'client commands' }
RowanAnsweringService >> subclassCreationTemplate: className [
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
]

{ #category : 'client commands' }
RowanAnsweringService >> transcriptInstalled [

	answer := self isTranscriptInstalled.
	RowanCommandResult addResult: self.
]

{ #category : 'private' }
RowanAnsweringService >> transcriptObject [
	
	^(SessionTemps current  at: #'TranscriptStream_SessionStream')
]

{ #category : 'client commands' }
RowanAnsweringService >> turnOffTranscriptWrites [

	self isTranscriptInstalled ifTrue:[
		self flipTranscript]
]
