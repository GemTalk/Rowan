Class {
	#name : 'RwSpecification',
	#superclass : 'Object',
	#instVars : [
		'version'
	],
	#category : 'Rowan-Specifications'
}

{ #category : 'instance creation' }
RwSpecification class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston'"

	"self fromUrl: 'sampleLoadSpec'"

	"self fromUrl: 'https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ 
			^ Rowan image
				loadedProjectNamed: specNameOrUrl
				ifAbsent: [ self error: 'No project named ' , specNameOrUrl printString , ' found' ] ].
	url scheme = 'file'
		ifTrue: [ 
			CypressFileUtilities current
				readStreamFor: url fileName
				in: url pathForDirectory
				do: [ :stream | 
					^ (STON fromStream: stream)
						initializeForImport;
						yourself ] ].
	url scheme asString = 'https'
		ifTrue: [ 
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ (STON fromString: response decodeFromUTF8)
				initializeForImport;
				yourself ].
	self error: 'Unknown scheme: ' , url scheme printString

]

{ #category : 'instance creation' }
RwSpecification class >> new [

	^self basicNew initialize
]

{ #category : 'private' }
RwSpecification >> currentVersion [
	"
		0.1.0 - initial version for specs
		0.2.0 - defaultConfigurationNames and defaultGroupNames i.v. added to RwProjectSpecification
	"
	^ '0.2.0'

]

{ #category : 'initialization' }
RwSpecification >> initialize [
  
]

{ #category : 'initialization' }
RwSpecification >> initializeForExport [
  "if spec has been exported, clear out any of the fields that represent local disk state"

]

{ #category : 'initialization' }
RwSpecification >> initializeForImport [

  "if spec has been imported, clear out any of the fields that represent local disk state"
  

]

{ #category : 'copying' }
RwSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  
]

{ #category : 'accessing' }
RwSpecification >> projectUrl: ignored [
  
]

{ #category : 'accessing' }
RwSpecification >> version [
	"answer the version of the class --- used to mark the version of the class for objects written to disk"

	"see currentVersion method for list of versions"

	"currently RwProjectSpecification is the only instance being versioned as it represents the version of the whole structure, 
		but I don't want to rule out the possiblity that other specs may need to be independently versioned."

	^ version

]
