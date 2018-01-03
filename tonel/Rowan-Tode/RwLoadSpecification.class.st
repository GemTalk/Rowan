Class {
	#name : 'RwLoadSpecification',
	#superclass : 'Object',
	#instVars : [
		'specName',
		'symbolDictName',
		'repositoryUrl',
		'userId',
		'methodEnv'
	],
	#classVars : [
		'Registry'
	],
	#classInstVars : [
		'knownRepositories',
		'configuration',
		'platform'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'instance creation' }
RwLoadSpecification class >> fromUrl: specNameOrUrl [
	"self fromUrl: 'file:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston'"
	"self fromUrl: 'sampleLoadSpec'"
	"self fromUrl: 'https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston'"
	| url |
  	url := specNameOrUrl asUrl.
	url schemeName isNil ifTrue: [ ^ self registry at: specNameOrUrl ].
	url schemeName = 'file' 
		ifTrue: [ 
			CypressFileUtilities current 
				readStreamFor: url fileName  
				in: url pathForDirectory 
				do: [:stream | ^ STON fromStream: stream ]].
	url schemeName = 'https'
		ifTrue: [ | contents client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := ZnClient new.
				response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
  			^ STON fromString: response decodeFromUTF8 ].
	self error: 'Unknown scheme: ', url scheme printString
]

{ #category : 'initialization' }
RwLoadSpecification class >> initialize [
	"self initialize"
	
	Registry := StringKeyValueDictionary new
]

{ #category : 'accessing' }
RwLoadSpecification class >> registry [

	^Registry
]

{ #category : 'private' }
RwLoadSpecification >> _myUserProfile [
  ^ System myUserProfile
]

{ #category : 'exporting' }
RwLoadSpecification >> exportToUrl: specNameOrUrl [

	| url |
  	url := specNameOrUrl asUrl.
	url schemeName isNil ifTrue: [ ^ self class registry at: specNameOrUrl put: self ].
	url schemeName = 'file' 
		ifTrue: [ 
			CypressFileUtilities current 
				writeStreamFor: self specName, '.ston'
				in: url pathForDirectory 
				do: [:stream |  | string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].

]

{ #category : 'loading' }
RwLoadSpecification >> loadUsingLibrarian: aRwLibrarian [

	self subclassResponsibility: #loadUsingLibrarian:
]

{ #category : 'accessing' }
RwLoadSpecification >> methodEnv [

   ^methodEnv ifNil: [ methodEnv := 0 ]

]

{ #category : 'accessing' }
RwLoadSpecification >> methodEnv: anObject [

   methodEnv := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	self subclassResponsibility: #packageOrGroupNamesToLoad:
]

{ #category : 'accessing' }
RwLoadSpecification >> repositoryUrl [

   ^repositoryUrl

]

{ #category : 'accessing' }
RwLoadSpecification >> repositoryUrl: anObject [

   repositoryUrl := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> specName [

   ^specName
]

{ #category : 'accessing' }
RwLoadSpecification >> specName: anObject [

   specName := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> symbolDictName [

   ^symbolDictName ifNil: [ symbolDictName := 'UserGlobals' ]

]

{ #category : 'accessing' }
RwLoadSpecification >> symbolDictName: anObject [

   symbolDictName := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> userId [

   ^userId ifNil: [ userId := self _myUserProfile userId ]

]

{ #category : 'accessing' }
RwLoadSpecification >> userId: anObject [

   userId := anObject

]
