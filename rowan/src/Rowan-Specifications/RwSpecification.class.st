Class {
	#name : 'RwSpecification',
	#superclass : 'Object',
	#category : 'Rowan-Specifications'
}

{ #category : 'accessing' }
RwSpecification class >> _supportedPlatformNames [
	^ #('gemstone' 'pharo' 'squeak' 'vast')
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
		ifTrue: [ 
			| downoadedFile cmd curlErrorFile |
			cmd := '/usr/bin/curl -L ' , url printString.
			downoadedFile := FileReference newTempFilePrefix: 'LoadSpec' suffix: '.ston'.
			curlErrorFile := FileReference newTempFilePrefix: 'CurlError' suffix: '.err'.
			[ 
			GsHostProcess new
				commandLine: cmd;
				stderrPath: curlErrorFile pathString;
				stdoutPath: downoadedFile pathString;
				executeWithInput: nil ]
				on: ChildError
				do: [ :ex | 
					self
						error:
							'error downloading url ' , url printString , ' see ' , curlErrorFile pathString
								, ' for details' ].
			[ 
			[ ^ self fromFile: downoadedFile pathString ]
				on: Error
				do: [ :ex | 
					self
						error:
							'error creating load spec' , url printString , ' see '
								, downoadedFile pathString , ' for details' ] ]
				ensure: [ 
					curlErrorFile ensureDelete.
					downoadedFile ensureDelete ] ].
	self error: 'Unknown scheme: ' , url scheme printString
]

{ #category : 'accessing' }
RwSpecification class >> label [
	^ self class name asString , ' '
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
