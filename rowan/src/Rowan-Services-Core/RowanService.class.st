"
Rowan Service is the abstract service class for classes that represent
first class entities in Rowan. They are transported to the client via 
ston. 

On the client, set the command & commandArgs inst vars, then tell
the browser to #issueCommand: with an array of services. A service
received without the the command inst var set will send #update to
the service. #issueCommand: should call JadeServer>>updateFromSton:
which will run the command and return a result. 

Any service that sends updates back to the client willl propogates to 
registered windows. Add a service for return to the client with:
RowanCommandResult addResult: <self>
"
Class {
	#name : 'RowanService',
	#superclass : 'Object',
	#instVars : [
		'command',
		'commandArgs',
		'updateType',
		'organizer',
		'updates',
		'wasUpdated'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'autocommit' }
RowanService class >> autoCommit [

	^SessionTemps current at: #'Jadeite_AutoCommit' ifAbsentPut: [false]
]

{ #category : 'autocommit' }
RowanService class >> breakPointsAreEnabled [

	^SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' ifAbsentPut: [true]
]

{ #category : 'rsr' }
RowanService class >> clientClassName [

	^self name
]

{ #category : 'autocommit' }
RowanService class >> flipAutoCommit [
	| newValue |
	newValue := self autoCommit == #failed ifTrue:[false] ifFalse:[self autoCommit not].
	^self setAutoCommit: newValue
]

{ #category : 'rsr' }
RowanService class >> isClientClass [
	
	^true
]

{ #category : 'rsr' }
RowanService class >> isServerClass [
	
	^true
]

{ #category : 'instance creation' }
RowanService class >> new [

	^super new initialize

]

{ #category : 'other' }
RowanService class >> rowanFixMe [

	"send this message to see everywhere that GS_Jade should be fixed"

]

{ #category : 'examples' }
RowanService class >> sampleService [

	^self new sampleService

]

{ #category : 'rsr' }
RowanService class >> serverClassName [

	^self name
]

{ #category : 'autocommit' }
RowanService class >> setAutoCommit: object [

	^SessionTemps current at: #'Jadeite_AutoCommit' put: object
]

{ #category : 'autocommit' }
RowanService class >> setBreakPointsAreEnabled: boolean [
  ^ SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' put: boolean
]

{ #category : 'rsr' }
RowanService class >> templateClassName [ 

	^self name
]

{ #category : 'accessing' }
RowanService class >> version [
  "change this method carefully and only at Jadeite release boundaries.
	Failure to do so will prevent logins"

  ^ 3200
]

{ #category : 'commands support' }
RowanService >> addAllSubclassHierarchiesOf: theClass to: hierarchies [
	(organizer subclassesOf: theClass)
		do: [ :subclass | 
			hierarchies
				add:
					(organizer allSuperclassesOf: theClass) , (Array with: theClass with: subclass).
			self addAllSubclassHierarchiesOf: subclass to: hierarchies ]
]

{ #category : 'other' }
RowanService >> answer: anObject [

	| answeringService |
	answeringService := RowanAnsweringService new. 
	answeringService answer: anObject. 
	RowanCommandResult addResult: answeringService.
]

{ #category : 'commands support' }
RowanService >> autoCommitIfRequired [
	| commitResult |
	self class autoCommit == true ifTrue:[
		commitResult := System commitTransaction.
		RowanAutoCommitService new autoCommit:  
			(commitResult 
				ifTrue:[true] 
				ifFalse:[#failed])].
]

{ #category : 'rowan' }
RowanService >> browserTool [

	^self projectTools browser

]

{ #category : 'commands support' }
RowanService >> classHierarchy: theClasses [
	| superclassChains levels services hierarchies toExpand hierarchyServices |
	superclassChains := self peerHierarchies: theClasses.
	hierarchies := self extendHierarchies: superclassChains forClasses: theClasses.
	levels := self hierarchiesByLevel: hierarchies.
	services := Dictionary new.
	toExpand := Set new.
	self
		services: services
		from: levels
		expand: toExpand
		classes: theClasses.
	hierarchyServices := services reject: [ :array | array isEmpty ].
	hierarchyServices copy
		keysAndValuesDo: [ :key :value | 
			hierarchyServices
				at: key
				put: (value asSet asSortedCollection: [ :x :y | x name < y name ]) asArray ].
	^ hierarchyServices
]

{ #category : 'rsr' }
RowanService >> clearOrganizers [
	organizer := nil.
	updates do: [ :update | update organizer: nil ]
]

{ #category : 'accessing' }
RowanService >> command [

	^command

]

{ #category : 'accessing' }
RowanService >> command: aSymbol [

	command := aSymbol

]

{ #category : 'accessing' }
RowanService >> commandArgs [

	^commandArgs ifNil:[commandArgs := Array new]

]

{ #category : 'accessing' }
RowanService >> commandArgs: anArray [

	"for tests" 

	commandArgs := anArray
]

{ #category : 'symbol dictionaries' }
RowanService >> createDefaultSymbolDictionary [

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'samples' }
RowanService >> createSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanService >> createSymbolDictionaryNamed: aName [

	| dictionary size |
	dictionary := SymbolDictionary new.
	dictionary at: aName asSymbol put: dictionary.
	size := Rowan image symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^ dictionary
]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionary [

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionaryName [

	^'RowanProjects'

]

{ #category : 'rowan' }
RowanService >> definitionClass [

	^self subclassResponsibility

]

{ #category : 'rowan' }
RowanService >> definitionClassName [

	^self definitionClass name

]

{ #category : 'replication' }
RowanService >> excludedInstVars [

	^#( #organizer)

]

{ #category : 'rsr' }
RowanService >> executeCommand [
	"RSR -> RowanServices primary api."

	[ 
	Rowan commandResultClass initializeResults.
	organizer := ClassOrganizer new.
	[ 
	updateType := nil.	"Update type is only for returned commands"
	command ifNil: [ command := #'update' ].
	GsFile gciLogClient: '========== about to execute command ============='.
	self servicePerform: command withArguments: commandArgs ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	GsFile gciLogClient: '========== about to execute post command ============='.
	Rowan loggingServiceClass current logSentServices.
	updates := Rowan commandResultClass results.
	self postCommandExecution ]
		on: Error
		do: [ :ex | 
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ].

	^ self
]

{ #category : 'commands support' }
RowanService >> extendHierarchies: hierarchies forClasses: theClasses [
	"extend the hierarchies by one level
	of subclasses if it is a selected class"

	hierarchies
		do: [ :hierarchy | 
			| theClass |
			theClass := hierarchy last.
			(theClasses includes: theClass)
				ifTrue: [ self addAllSubclassHierarchiesOf: theClass to: hierarchies ] ].
	^ hierarchies
]

{ #category : 'perform' }
RowanService >> handleDeletedService [
  self updateType: #'removed:'.
  RowanCommandResult addResult: self
]

{ #category : 'commands support' }
RowanService >> hierarchiesByLevel: hierarchies [

	"Return dictionary of classes by level. 
	Example: 
		hierarchies - #(#(Object Collection Array) #(Object AbstractException Exception))
	Return: 
		#(#nil->#(Object) Object->#(Collection AbstractException) Collection->#(Array) AbstractException->#(Exception)
	"
	| levels |
	levels := hierarchies inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls theSuper classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			((dict at: #'nil') includes: cls) ifFalse:[(dict at: #'nil') add: cls].
		]
		ifFalse:[
				theSuper := chain at: index - 1.
				((dict at: theSuper) includes: cls) ifFalse:[(dict at: theSuper) add: cls]
				]].
			dict].
	^levels
]

{ #category : 'initialization' }
RowanService >> initialize [
	updates := Array new.
	wasUpdated := false
]

{ #category : 'testing' }
RowanService >> isClassService [

	^false
]

{ #category : 'testing' }
RowanService >> isDefinedProject [

	^false
]

{ #category : 'testing' }
RowanService >> isDictionaryService [

	^false
]

{ #category : 'testing' }
RowanService >> isMethodService [

	^false
]

{ #category : 'testing' }
RowanService >> isPackageService [

	^false
]

{ #category : 'testing' }
RowanService >> isProjectService [

	^false
]

{ #category : 'testing' }
RowanService >> isUpdating [

	^command == #update
]

{ #category : 'perform' }
RowanService >> isUpdatingButFoundToBeDeleted [
  ^ self command == #'update' and: [ self wasDeleted ]
]

{ #category : 'accessing' }
RowanService >> jadeiteServer [

	^(Rowan jadeServerClassNamed: #JadeServer) theJadeiteServer
]

{ #category : 'accessing' }
RowanService >> organizer: anOrganizer [

	organizer := anOrganizer.
]

{ #category : 'commands support' }
RowanService >> peerHierarchies: theClasses [
	" create hierarchies for each peer of the 
	classes of interest."

	| peerHierarchies |
	peerHierarchies := Array new.
	theClasses
		do: [ :theClass | 
			| superclass |
			superclass := theClass superclass.
			superclass
				ifNil: [ peerHierarchies add: (Array with: theClass) ]
				ifNotNil: [ 
					superclass subclasses
						do: [ :subclass | 
							peerHierarchies
								add:
									((organizer allSuperclassesOf: subclass)
										add: subclass;
										yourself) ] ] ].
	^ peerHierarchies
]

{ #category : 'rsr' }
RowanService >> postCommandExecution [
	
	self clearOrganizers. 
	updates remove: self ifAbsent: [  ].
	self autoCommitIfRequired
]

{ #category : 'rowan' }
RowanService >> projectTools [

	^Rowan projectTools

]

{ #category : 'release' }
RowanService >> releaseProcessOop: oop [
  "not really releasing it. The client should have registered
	the process with the debugger window it opened before
	this is run"

  | jadeiteProcesses process |
  ((process := Object _objectForOop: oop) isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      jadeiteProcesses remove: process ifAbsent: [  ] ]
]

{ #category : 'samples' }
RowanService >> removeSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.

]

{ #category : 'symbol dictionaries' }
RowanService >> removeSymbolDictionaryNamed: aName [

	| index |
	index := Rowan image symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
]

{ #category : 'other' }
RowanService >> rowanFixMe [
		
	"marker for all things broken in Rowan"

]

{ #category : 'accessing' }
RowanService >> rowanProjectName [

	"all services should be able to return a project name
	even if they are not truly packaged" 

	^nil
]

{ #category : 'samples' }
RowanService >> sampleSymbolDictionaryName [

	^'SampleSymbolDictionaryName'

]

{ #category : 'perform' }
RowanService >> servicePerform: symbol withArguments: collection [
	"subclasses may not want to update after performing the command"

	self servicePerform: symbol withArguments: collection shouldUpdate: true
]

{ #category : 'perform' }
RowanService >> servicePerform: symbol withArguments: collection shouldUpdate: shouldUpdate [
	"each service updates itself after performing a command.
	Therefore, if the command is #update, don't run it here"

	SessionTemps current
		at: #'versionsVerified'
		ifAbsent: [ 
			SessionTemps current at: #'versionsVerified' put: false.
			self
				inform:
					'Version mismatch failure. Client version is older than server version.' ].
	super perform: symbol withArguments: collection.
	shouldUpdate
		ifTrue: [ self update ]
]

{ #category : 'commands support' }
RowanService >> services: services from: levels expand: toExpand classes: theClasses [
  "In order to avoid the expense of creating duplicate services, we cache
them in the newServices temporary for look up"

  | newServices |
  newServices := Array new.
  theClasses
    do: [ :aClass | toExpand addAll: (organizer allSuperclassesOf: aClass) ].
  levels
    keysAndValuesDo: [ :key :value | 
      | newKey service |
      newKey := key = #'nil'
        ifTrue: [ #'nil' ]
        ifFalse: [ 
          service := newServices
            detect: [ :classService | classService name = key name ]
            ifNone: [ RowanClassService new classServiceFromOop: key asOop ].
          (toExpand includes: service theClass)
            ifTrue: [ service expand: true ]
            ifFalse: [ service expand: false ].
          service ].
      services
        at: newKey
        put:
          (value
            collect: [ :cls | 
              service := newServices
                detect: [ :classService | classService name = cls name ]
                ifNone: [ RowanClassService new classServiceFromOop: cls asOop ].
              (toExpand includes: service theClass)
                ifTrue: [ service expand: true ]
                ifFalse: [ service expand: false ].
              service ]) ]
]

{ #category : 'replication' }
RowanService >> stonOn: stonWriter [
    | instanceVariableNames |
    instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
    stonWriter writeObject: self
        streamMap: 
            [:dictionary |
            instanceVariableNames do: 
                    [:each |
                    (self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
                        ifNotNil: [:value | dictionary at: each asSymbol put: value]
                        ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
]

{ #category : 'replication' }
RowanService >> stonStringFor: anObject [

	"return a string representing a complete object structure
	suitable for replicating on the client."

	^STON toString: anObject

]

{ #category : 'private' }
RowanService >> stripOutUnicode: string [
  | asciiString |
  asciiString := string
    collect: [ :char | 
      ((self validLowRangeCharacters includes: char) not
        and: [ char asciiValue < 32 or: [ char asciiValue > 255 ] ])
        ifTrue: [ $? ]
        ifFalse: [ char ] ].
  ^ asciiString
]

{ #category : 'commands support' }
RowanService >> superclassChainsFor: behaviors [
	organizer := ClassOrganizer new. 
	^behaviors collect:[:behavior | | supers |
			supers := organizer allSuperclassesOf: behavior. 
			supers add: behavior. 
			supers].

]

{ #category : 'symbol dictionaries' }
RowanService >> symbolDictionaryNamed: aName [

	| symbolList  index |
	symbolList := Rowan image symbolList.
	index :=symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].

]

{ #category : 'update' }
RowanService >> update [
	wasUpdated := true
]

{ #category : 'update' }
RowanService >> updateInternalService: updatedService [

	"no internally held services to update"
]

{ #category : 'update' }
RowanService >> updateLatest [
  "subclasses may want to special behavior to update themselves
	to their loaded version"

  self update
]

{ #category : 'accessing' }
RowanService >> updateType: aSymbol [

	updateType := aSymbol
]

{ #category : 'accessing' }
RowanService >> userGlobals [
  ^ Rowan image symbolList objectNamed: #'UserGlobals'
]

{ #category : 'private' }
RowanService >> validLowRangeCharacters [
  ^ Array with: Character lf with: Character tab
]

{ #category : 'testing' }
RowanService >> wasDeleted [

	^false
]

{ #category : 'fileout' }
RowanService >> writeFileOutHeaderOn: stream [
	"This method will write a fileout header onto the given file.
	Adapted from GBS - GbxBrowser>>writeFileOutHeaderOn:"

	| rawVer beVer cr |
	stream nextPutAll: 'fileformat utf8';
			cr. 
	rawVer := System _version.
	beVer := ''.
	cr := String with: Character cr.
	"Comment each newline"
	(rawVer subStrings: (Array with: Character lf)) do: [:line | beVer := beVer , '! ' , line , cr].
	stream
		nextPutAll: '!';
		cr;
		nextPutAll: '! From ';
		nextPutAll: beVer;
		cr;
		nextPutAll: '! On ';
		nextPutAll: Date today printString;
		nextPutAll: ', ';
		nextPutAll: Time now printString;
		cr;
		nextPutAll: '!';
		cr;
		flush
]
