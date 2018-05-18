Class {
	#name : 'RwAbstractProjectConfiguration',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwAbstractProjectConfiguration class >> _readStonFrom: stream [

	| reader | 
	(reader := STONReader on: stream)
		allowComplexMapKeys: true.
	^reader next
		initializeForImport;
		yourself

]

{ #category : 'instance creation' }
RwAbstractProjectConfiguration class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ 
			CypressFileUtilities current
				readStreamFor: url fileName
				in: url pathForDirectory
				do: [ :stream | ^ self _readStonFrom: stream ] ].
	url scheme asString = 'https'
		ifTrue: [ 
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (System myUserProfile symbolList objectNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8 ].
	self error: 'Unknown scheme: ' , url scheme printString

]

{ #category : 'instance creation' }
RwAbstractProjectConfiguration class >> newNamed: aName for: projectName [

	^ self new
		initialize;
		name: aName;
		projectName: projectName;
		yourself
]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _configsPath [

	^ self _specification configsPath
]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _loadedProject [

	^ Rowan image loadedProjectNamed: self projectName
]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _repositoryRootPath [

	^ self _specification repositoryRootPath
]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _repositoryUrl [

	^ self _specification repositoryUrl
]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _specification [

	^ self _loadedProject specification
]

{ #category : 'visiting' }
RwAbstractProjectConfiguration >> acceptVisitor: aVisitor [

	^ self subclassResponsibility: #acceptVisitor:

]

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> comment [

   ^comment

]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> comment: anObject [

   comment := anObject

]

{ #category : 'exporting' }
RwAbstractProjectConfiguration >> export [

	| url exportUrl exportPath export |
	self _repositoryUrl
		ifNil: [ self error: 'exportToUrl: must be used if repositoryUrl has not been set' ].
	url := RwUrl fromString: self _repositoryUrl.
	exportPath := self _repositoryRootPath , '/' , self _configsPath , '/'.
	Rowan fileUtilities ensureDirectoryExists: exportPath.
	exportUrl := 'file:' , exportPath.

	export := self copy.
	export initializeForExport.
	^ export exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwAbstractProjectConfiguration >> exportToUrl: fileUrl [

	| url |
	url := RwUrl fromString: fileUrl.
	url schemeName = 'file'
		ifTrue: [ 
			Rowan fileUtilities
				writeStreamFor: self name , '.ston'
				in: url pathForDirectory
				do: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	^ nil	"otherwise a noop"
]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> initialize [

	comment := ''
]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> initializeForExport [

	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	self initializeForImport
]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> initializeForImport [

	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil
]

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> name [

   ^name

]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> name: anObject [

   name := anObject

]

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> projectName [

   ^projectName

]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> projectName: anObject [

   projectName := anObject

]

{ #category : 'load support' }
RwAbstractProjectConfiguration >> resolvePackageNames [

	"return the list of package names that is the result of resolving all of the receiver's references"

	self subclassResponsibility: #'resolvePackageNames'
]

{ #category : 'load support' }
RwAbstractProjectConfiguration >> resolvePackageNames: visitedConfigurationNames [

	"return the list of package names that is the result of resolving all of the receiver's references.
		Do not visit configurations already listed in visitedConfigurationNames "

	self subclassResponsibility: #'resolvePackageNames:'
]
