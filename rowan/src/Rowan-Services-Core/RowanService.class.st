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
		'definition',
		'updates',
		'command',
		'commandArgs',
		'updateType',
		'organizer'
	],
	#category : 'Rowan-Services-Core'
}

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

{ #category : 'rowan' }
RowanService >> browserTool [

	^self projectTools browser

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
	size := System myUserProfile symbolList size.
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

{ #category : 'initialization' }
RowanService >> initialize [
]

{ #category : 'accessing' }
RowanService >> jadeiteServer [

	^JadeServer theJadeiteServer
]

{ #category : 'accessing' }
RowanService >> organizer: anOrganizer [

	organizer := anOrganizer.
]

{ #category : 'rowan' }
RowanService >> projectTools [

	^Rowan projectTools

]

{ #category : 'samples' }
RowanService >> removeSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.

]

{ #category : 'symbol dictionaries' }
RowanService >> removeSymbolDictionaryNamed: aName [

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]

]

{ #category : 'other' }
RowanService >> rowanFixMe [
		
	"marker for all things broken in Rowan"

]

{ #category : 'rowan' }
RowanService >> rowanLoadedPackageNames [

	| stream packages |
	self rowanFixMe.	"handle modified package display"
	stream := WriteStream on: String new.
	packages := Rowan packageNames.
	packages do: 
					[:package |
					stream
						nextPutAll: package;
						tab;
						nextPut: ((RwPackage newNamed: package) isDirty ifTrue:[$Y] ifFalse:[$N]);
						tab;
						nextPutAll: package;
						lf].
	^stream contents

]

{ #category : 'samples' }
RowanService >> sampleSymbolDictionaryName [

	^'SampleSymbolDictionaryName'

]

{ #category : 'perform' }
RowanService >> servicePerform: symbol withArguments: collection [
	super perform: symbol withArguments: collection.
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

{ #category : 'accessing' }
RowanService >> updateType: aSymbol [

	updateType := aSymbol
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
