Class {
	#name : 'RwSpecification',
	#superclass : 'Object',
	#category : 'Rowan-Specifications'
}

{ #category : 'accessing' }
RwSpecification class >> _supportedPlatformNames [
	^ #('gemstone' 'vast' 'pharo')
]

{ #category : 'instance creation' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ (STON fromStream: stream)
				initializeForImport;
				yourself ]
]

{ #category : 'instance creation' }
RwSpecification class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ ^ self fromFile: url pathForFile ].
	url scheme asString = 'https'
		ifTrue: [ self error: 'not yet supported'.
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
RwSpecification class >> new [

	^self basicNew initialize
]

{ #category : 'conversion' }
RwSpecification >> asSpecification [

	^ self
]

{ #category : 'private' }
RwSpecification >> currentVersion [
	"
		0.1.0 - initial version for specs
		0.2.0 - defaultConfigurationNames and defaultGroupNames i.v. added to RwProjectSpecification
		0.3.0 - remoteUrl i.v. added to RwGitRepositorySpecification
	"
	^ '0.3.0'
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

	^ nil
]
