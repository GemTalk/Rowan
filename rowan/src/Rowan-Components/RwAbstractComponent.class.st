Class {
	#name : 'RwAbstractComponent',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'componentNames',
		'packageNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwAbstractComponent class >> _readStonFrom: stream [
	| reader component |
	(reader := STONReader on: stream) allowComplexMapKeys: true.
	component := reader next
		initializeForImport;
		yourself.
	component validate.	"validate when reading from disk, since hand editting could create inconsistencies"
	^ component
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromComponentsDirectory: componentsDirectory named: componentName [
	| component url |
	url := 'file:' , (componentsDirectory / componentName , 'ston') pathString.
	component := self fromUrl: url.
	component _readDoitsFrom: componentsDirectory.
	component _validateDoits.
	^ component
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			Rowan projectTools trace trace: '--- reading component ' , filePath asString.
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ self _readStonFrom: stream ]
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ ^ self fromFile: url pathForFile ].
	url scheme asString = 'https'
		ifTrue: [ 
self error: 'not yet supported'.
"
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8
" ].
	self error: 'Unknown scheme: ' , url scheme printString
]

{ #category : 'instance creation' }
RwAbstractComponent class >> new [

	^self basicNew initialize

]

{ #category : 'instance creation' }
RwAbstractComponent class >> newNamed: aName [

	^ self new
		name: aName;
		yourself
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNamed: aComponentName [ 
	"avoid duplicates and keep the list sorted"
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNames: aComponentNames [
	"add component names"

	self componentNames addAll: aComponentNames.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> comment [

   ^comment

]

{ #category : 'initialization' }
RwAbstractComponent >> comment: anObject [

   comment := anObject

]

{ #category : 'accessing' }
RwAbstractComponent >> componentNames [

	^ componentNames
]

{ #category : 'initialization' }
RwAbstractComponent >> initialize [
	comment := ''.
	packageNames := {}.
	componentNames := {}
]

{ #category : 'accessing' }
RwAbstractComponent >> name [

   ^name

]

{ #category : 'initialization' }
RwAbstractComponent >> name: anObject [

   name := anObject

]

{ #category : 'accessing' }
RwAbstractComponent >> packageNames [

	^ packageNames
]

{ #category : 'printing' }
RwAbstractComponent >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPutAll: name.
]
