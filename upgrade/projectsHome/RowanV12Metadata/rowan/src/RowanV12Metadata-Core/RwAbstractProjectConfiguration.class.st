Class {
	#name : 'RwAbstractProjectConfiguration',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'version',
		'configurationNames'
	],
	#category : 'RowanV12Metadata-Core'
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
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8 ].
	self error: 'Unknown scheme: ' , url scheme printString

]

{ #category : 'instance creation' }
RwAbstractProjectConfiguration class >> new [

	^self basicNew initialize

]

{ #category : 'instance creation' }
RwAbstractProjectConfiguration class >> newNamed: aName for: projectName [

	^ self new
		name: aName;
		projectName: projectName;
		yourself

]

{ #category : 'private' }
RwAbstractProjectConfiguration >> _configsPath [

	^ self _specification configsPath
]

{ #category : 'exporting' }
RwAbstractProjectConfiguration >> _exportToUrl: fileUrl [

	| url |
	url := fileUrl asRwUrl.
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
RwAbstractProjectConfiguration >> acceptCompoundVisitor: aVisitor [

	^ self subclassResponsibility: #acceptCompoundVisitor:

]

{ #category : 'visiting' }
RwAbstractProjectConfiguration >> acceptNestedVisitor: aVisitor [

	^ self subclassResponsibility: #acceptNestedVisitor:

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

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> configurationNames [

   ^ configurationNames ifNil: [ Array new ]

]

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> configurationNames: anObject [

   configurationNames := anObject

]

{ #category : 'private' }
RwAbstractProjectConfiguration >> currentVersion [

	"
		0.1.0 - initial version for specs
	"

	"concrete subclasses, will override as needed"

	^ '0.1.0'

]

{ #category : 'exporting' }
RwAbstractProjectConfiguration >> export [

	|  exportUrl exportPath |
	exportPath := self _repositoryRootPath , '/' , self _configsPath , '/'.
	Rowan fileUtilities ensureDirectoryExists: exportPath.
	exportUrl := 'file:' , exportPath.
	^ self exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwAbstractProjectConfiguration >> exportToUrl: fileUrl [

	^ self copy
		initializeForExport;
		_exportToUrl: fileUrl

]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> initialize [

	comment := ''.
	version := self currentVersion.

]

{ #category : 'initialization' }
RwAbstractProjectConfiguration >> initializeForExport [

	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil

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

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> version [
	"answer the version of the class --- used to mark the version of the class for objects written to disk"

	"see currentVersion method for list of versions"

	^ version

]

{ #category : 'accessing' }
RwAbstractProjectConfiguration >> version: aString [

	self error: 'Do not use version: ... the version should only be set when read from disk using STON'

]
