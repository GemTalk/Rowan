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

{ #category : 'private' }
RowanAnsweringService >> addLowerCaseSymbolsIn: theClass To: array [
  array addAll: theClass selectors.
  array addAll: theClass class selectors.
  array addAll: theClass instVarNames.
  array addAll: theClass class instVarNames.
  array addAll: theClass classVarNames.
]

{ #category : 'client commands' }
RowanAnsweringService >> allClassesStartingWith: string [
  answer := SortedCollection new.
  organizer classes
    do: [ :cls | 
      (cls name beginsWith: string)
        ifTrue: [ answer add: cls name asString ] ].
  answer := answer asArray.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> allClassNames [
  answer := SortedCollection new.
  answer addAll: (organizer classes collect: [ :cls | cls name asString ]).
  answer := answer asArray.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> allTestsIn: classServices [
	answer := Array new. 
	classServices do:[:service | answer addAll: service allTests].
	RowanCommandResult addResult: self.
]

{ #category : 'accessing' }
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
RowanAnsweringService >> autocompleteSymbols [
  | newClassNames newLowerCaseSymbols |
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  organizer classes
    do: [ :cls | 
      newClassNames add: cls name asString.
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  SessionTemps current
    at: #'autocompleteSymbolCache'
    put: (Array with: newClassNames with: newLowerCaseSymbols).
  answer := Array
    with: newClassNames asOrderedCollection
    with: newLowerCaseSymbols asOrderedCollection.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
]

{ #category : 'client command support' }
RowanAnsweringService >> basicExec: aString context: oop [
	answer := [true -> (aString evaluateInContext: (Object _objectForOop: oop) symbolList: Rowan image symbolList) asOop] 
		on: CompileError do: [:ex | 
			false -> ex errorDetails
	].
	answer key ifTrue:[(RowanService autoCommit == true) ifTrue:[System commitTransaction]].
	^answer
]

{ #category : 'client command support' }
RowanAnsweringService >> basicMethodHistoryFor: methodService [
  | rowanMethodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  answer := (rowanMethodHistory at: methodService ifAbsentPut: [ Array new ])
    asOrderedCollection.
  answer
    addFirst: (rowanMethodHistory keys detect: [ :svc | svc = methodService ])
]

{ #category : 'client command support' }
RowanAnsweringService >> basicPrintStringOf: oop toMaxSize: integer [
  | object |
  object := Object _objectForOop: oop.
  ^ self basicPrintStringOfObject: object toMaxSize: integer
]

{ #category : 'client command support' }
RowanAnsweringService >> basicPrintStringOfObject: object toMaxSize: integer [
  "avoid the oop conversion when we already have the object"

  | printString |
  printString := self stripOutUnicode: object printString.
  printString := printString size > integer
    ifTrue: [ (printString copyFrom: 1 to: integer) , '...' ]
    ifFalse: [ printString ].
  ^ printString
]

{ #category : 'client command support' }
RowanAnsweringService >> basicSortedSelectors [
  | selectors |
  selectors := IdentitySet new.
  organizer classes
    do: [ :aClass | 
      | metaClass |
      metaClass := aClass.
      2
        timesRepeat: [ 
          | methodDictionary |
          methodDictionary := metaClass _fullMethodDictEnv0.
          methodDictionary
            valuesDo: [ :method | 
              | selector |
              selector := method selector.
              selector charSize = 1
                ifTrue: [ selectors add: selector ].
              method _selectorPool
                do: [ :sentSelector | 
                  sentSelector charSize = 1
                    ifTrue: [ selectors add: sentSelector ] ] ].
          metaClass := metaClass class ] ].
  ^ selectors asSortedCollection asArray
]

{ #category : 'client command support' }
RowanAnsweringService >> basicSortedSymbols [
  | sortedSymbols |
  sortedSymbols := SortedCollection new.
  ((AllUsers userWithId: #'SymbolUser') resolveSymbol: #'AllSymbols') value
    keysDo: [ :symbol | 
      symbol charSize = 1
        ifTrue: [ sortedSymbols add: symbol ] ].
  ^sortedSymbols asArray
]

{ #category : 'client commands' }
RowanAnsweringService >> breakPointsAreEnabled [
  answer := RowanService breakPointsAreEnabled.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
]

{ #category : 'client commands' }
RowanAnsweringService >> canAccessServiceClasses [
  "Being able to see RowanLoggingService is a good
	indication that all of the service classes are visible"

  answer := [ '[RowanLoggingService] value. true.' evaluate ]
    on: Error
    do: [ :ex | false ].
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> classHasSubclasses: oop [
  | behavior |
  behavior := Object _objectForOop: oop.
  answer := behavior subclasses notEmpty.
  RowanCommandResult addResult: self
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
RowanAnsweringService >> disableMethodBreaks: methodServices [
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        disableMethodBreaks ]
]

{ #category : 'client commands' }
RowanAnsweringService >> doClientAndServerVersionsMatch: clientVersion [
  "Not to be sent through services so return an answer directly.
	Sent immediately after Jadeite login"

  SessionTemps current at: #'versionsVerified' put: false.
  clientVersion = RowanService version
    ifTrue: [ 
      answer := true.
      SessionTemps current at: #'versionsVerified' put: true ]
    ifFalse: [ 
      answer := clientVersion > RowanService version
        ifTrue: [ 
          'Client (' , clientVersion printString , ') is more recent than server ('
            , RowanService version printString , ')' ]
        ifFalse: [ 
          'Server (' , RowanService version printString , ') is more recent than client ('
            , clientVersion printString , ')' ] ].
  ^ answer
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
RowanAnsweringService >> enableMethodBreaks: methodServices [
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        enableMethodBreaks ]
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString [

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString context: oop [

	answer := self basicExec: aString context: oop. 
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString context: oop inWindow: handle [
  answer := self exec: aString context: oop.
  answer key
    ifTrue: [ RowanBrowserService new saveRootObject: answer value windowHandle: handle ].	"return answer for testing"
  ^ answer
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString inFrame: level ofProcess: processOop context: oop [
  | symbolList frameContents symbolDictionary process |
  symbolList := Rowan image symbolList.
  process := Object _objectForOop: processOop.
  process _isTerminated
    ifTrue: [ 
      RowanCommandResult addResult: self.
      ^ self ].
  frameContents := process _frameContentsAt: level.
  frameContents
    ifNotNil: [ 
      symbolDictionary := SymbolDictionary new.
      1 to: (frameContents at: 9) size do: [ :index | 
        ((frameContents at: 9) at: index) first = $.
          ifFalse: [ 
            symbolDictionary
              at: ((frameContents at: 9) at: index) asSymbol
              put: (frameContents at: 11 + index - 1) ] ].
      symbolList add: symbolDictionary before: symbolList first ].
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
              value: (symbolDictionary at: (argsAndTemps at: index)) printString
              className: (frameContents at: 8) class name asString.
            RowanCommandResult addResult: variableService ] ].
      symbolList remove: symbolDictionary ].	"return answer for testing"
  ^ answer
]

{ #category : 'client commands' }
RowanAnsweringService >> expressionSelector: string [
  "try to uncover a selector in the string. 
	If it's not possible, return the string and 
	let the application do with it what it will"

  | messageNode |
  messageNode := [ RBParser parseExpression: string ]
    on: Error
    do: [ :ex | nil ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: string , ' #foo' ]
        on: Error
        do: [ :ex | nil ] ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: '#foo ' , string ]
        on: Error
        do: [ :ex | nil ] ].
  answer := messageNode
    ifNil: [ string ]
    ifNotNil: [ 
      messageNode isMessage
        ifTrue: [ messageNode buildSelector ]
        ifFalse: [ 
          messageNode isCascade
            ifTrue: [ messageNode messages first buildSelector]
            ifFalse: [ string ] ] ].
  RowanCommandResult addResult: self.
  ^ answer	"return answer for testing"
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
RowanAnsweringService >> initializeBreakPointsAreEnabled [
  RowanService setBreakPointsAreEnabled: true
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
RowanAnsweringService >> isVariable: oop [
	| anObject |
	anObject := Object _objectForOop: oop. 
	answer := anObject class isVariable.
	RowanCommandResult addResult: self
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
RowanAnsweringService >> lowercaseSelectorsMatching: lowercaseSymbol [
  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> methodHistoryFor: methodService [
  self basicMethodHistoryFor: methodService.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
]

{ #category : 'client commands' }
RowanAnsweringService >> methodReferenceCounts: methodServices [
  answer := Array new.
  methodServices
    do: [ :methodService | answer add: (organizer sendersOf: methodService selector) first size ].
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
]

{ #category : 'client commands' }
RowanAnsweringService >> needsCommit [

	answer := System needsCommit.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> newPackageNamed: packageName [
  | packageService |
  packageService := RowanPackageService new name: packageName.
  packageService update.
  answer := packageService.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> printStringOf: oop toMaxSize: integer [
  answer := self basicPrintStringOf: oop toMaxSize: integer.
  RowanCommandResult addResult: self.
  ^ answer
]

{ #category : 'client commands' }
RowanAnsweringService >> printStringOfOops: oops toMaxSize: integer [
  answer := Array new.
  oops
    do: [ :oop | answer add: (self basicPrintStringOf: oop toMaxSize: integer) ].
  RowanCommandResult addResult: self.
  ^ answer
]

{ #category : 'client commands' }
RowanAnsweringService >> profile: block [
  "not used yet. Utility method needs testing.
	Make sure block execution time is long enough - say 1 second. 
	Results may not be consistent

	Usage example: 
| block answeringService | 
block := [| browserService profMonitor |
		browserService := (RowanBrowserService new) .
		10 timesRepeat:[browserService packagesWithTests]]. 
answeringService := RowanAnsweringService new profile: block.
answeringService answer. "

  | time ns |
  time := System millisecondsToRun: block.
  ns := ProfMonitor computeInterval: time / 1000.
  answer := ProfMonitor monitorBlock: block intervalNs: ns.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> removeMethodHistoryFor: methodService [
  | rowanMethodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  rowanMethodHistory removeKey: methodService ifAbsent: [  ]
]

{ #category : 'client commands' }
RowanAnsweringService >> resolveAsService: name [
  | projectService packageService classService dictionaryService |
  projectService := RowanProjectService new name: name.
  projectService update projectIsLoaded
    ifTrue: [ 
      answer := projectService.
      ^ RowanCommandResult addResult: self ].
  packageService := RowanPackageService new name: name.
  packageService update projectName
    ifNotNil: [ 
      answer := packageService.
      ^ RowanCommandResult addResult: self ].
  dictionaryService := RowanDictionaryService new name: name.
  dictionaryService update classes notEmpty
    ifTrue: [ 
      answer := dictionaryService.
      ^ RowanCommandResult addResult: self ].
  classService := RowanClassService new name: name.
  classService update projectName
    ifNotNil: [ 
      answer := classService.
      ^ RowanCommandResult addResult: self ].
  answer := nil.
  RowanCommandResult addResult: self
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
RowanAnsweringService >> selectorsMatching: lowercaseSymbol [
  "assume we're passed a lower case symbol to avoid
	case sensitive misses"

  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> selectorsMatchingPattern: pattern [
  answer := self basicSortedSelectors.
  answer := answer select: [ :each | each _matchPatternNoCase: pattern ].
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> setAutoCommit: object [

	answer := RowanService setAutoCommit: object.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> setBreakPointsAreEnabled: boolean [
  boolean
    ifTrue: [ self enableAllBreaks ]
    ifFalse: [ self disableAllBreaks ].
  RowanService setBreakPointsAreEnabled: boolean.
  answer := boolean.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> setEnableInteractionHandler: boolean [
  SessionTemps current at: #'rowanServiceInteractionActive' put: boolean
]

{ #category : 'client commands' }
RowanAnsweringService >> sortedSelectors [
  answer := self basicSortedSelectors.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> sortedSymbols [
  answer := self basicSortedSymbols. 
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> subclassCreationTemplate: className [
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
]

{ #category : 'client commands' }
RowanAnsweringService >> symbolExists: aSymbol [
  answer := (Rowan image symbolList resolveSymbol: aSymbol) isNil not.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanAnsweringService >> symbolsMatching: string [
  answer := self basicSortedSymbols
    select: [ :symbol | string sunitMatch: symbol ].
  RowanCommandResult addResult: self
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

{ #category : 'client commands' }
RowanAnsweringService >> updateAutocompleteSymbols [
  | cache newClassNames newLowerCaseSymbols |
  cache := SessionTemps current
    at: #'autocompleteSymbolCache'
    ifAbsent: [ ^ self	"autocomplete not activated" ].
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  organizer classes
    do: [ :cls | 
      (cache first includes: cls name asString)
        ifFalse: [ newClassNames add: cls name asString ].
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  (SessionTemps current at: #'autocompleteSymbolCache') first
    addAll: newClassNames.
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  cache last
    do: [ :selector | 
      (newLowerCaseSymbols includes: selector)
        ifTrue: [ newLowerCaseSymbols remove: selector ] ].
  (SessionTemps current at: #'autocompleteSymbolCache') last
    addAll: newLowerCaseSymbols.
  answer := Array with: newClassNames with: newLowerCaseSymbols.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
]
