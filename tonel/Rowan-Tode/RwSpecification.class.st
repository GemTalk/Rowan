Class {
	#name : 'RwSpecification',
	#superclass : 'Object',
	#instVars : [
		'specName'
	],
	#classVars : [
		'Registry'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'instance creation' }
RwSpecification class >> fromUrl: specNameOrUrl [
  "self fromUrl: 'file:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston'"

  "self fromUrl: 'sampleLoadSpec'"

  "self fromUrl: 'https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston'"

  | url |
  url := CypUrl fromString: specNameOrUrl.
  url schemeName isNil
    ifTrue: [ ^ self registry at: specNameOrUrl ].
  url schemeName = 'file'
    ifTrue: [ 
      CypressFileUtilities current
        readStreamFor: url fileName
        in: url pathForDirectory
        do: [ :stream | ^ STON fromStream: stream ] ].
  url schemeName = 'https'
    ifTrue: [ 
      | contents client response |
      GsSecureSocket disableCertificateVerificationOnClient.
      client := ZnClient new.
      response := client
        beOneShot;
        enforceHttpSuccess: true;
        get: url.
      ^ (STON fromString: response decodeFromUTF8) initializeForHttpsDownload ].
  self error: 'Unknown scheme: ' , url scheme printString
]

{ #category : 'initialization' }
RwSpecification class >> initialize [
	"self initialize"
	
	Registry := StringKeyValueDictionary new
]

{ #category : 'instance creation' }
RwSpecification class >> new [

	^self basicNew initialize
]

{ #category : 'accessing' }
RwSpecification class >> registry [

	^Registry
]

{ #category : 'initialization' }
RwSpecification >> initialize [
  
]

{ #category : 'initialization' }
RwSpecification >> initializeForHttpsDownload [
	"if spec has been downloaded, clear out any of the fields that represent local disk state"

]

{ #category : 'actions' }
RwSpecification >> packageNamesForTool: aRwTool [
  self subclassResponsibility: #'packageNamesForTool:'
]

{ #category : 'accessing' }
RwSpecification >> projectUrl: ignored [
  
]

{ #category : 'registering' }
RwSpecification >> register [
  self class registry at: self specName put: self
]

{ #category : 'accessing' }
RwSpecification >> specName [

   ^specName
]

{ #category : 'accessing' }
RwSpecification >> specName: anObject [

   specName := anObject

]

{ #category : 'registering' }
RwSpecification >> unregister [
  self class registry removeKey: self specName
]
