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

{ #category : 'ston' }
RwSpecification >> excludedInstVars [
	"restore full #instVarNamesInOrderForSton - no exclusions"

	^ #(  )
]

{ #category : 'ston' }
RwSpecification >> fromSton: stonReader [
	"Decode non-variable classes from a map of their instance variables and values."

	self class isVariable
		ifTrue: [ self subclassResponsibility ]
		ifFalse: [ 
			| instanceVariableNames |
			"ignore any incoming instance variables that aren't included in instVarNamesInOrderForSton"
			instanceVariableNames := self instVarNamesInOrderForSton.
			stonReader
				parseMapDo: [ :instVarName :value | 
					| index |
					index := instanceVariableNames indexOf: instVarName asSymbol.
					index > 0
						ifTrue: [ self instVarAt: index put: value ] ] ]

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

{ #category : 'ston' }
RwSpecification >> instVarNamesInOrderForSton [

	^ self class allInstVarNames
]

{ #category : 'copying' }
RwSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  
]

{ #category : 'accessing' }
RwSpecification >> projectUrl: ignored [
  
]

{ #category : 'ston' }
RwSpecification >> stonOn: stonWriter [
	| instanceVariableNames allInstanceVariableNames |
	instanceVariableNames := self instVarNamesInOrderForSton
		reject: [ :iv | self excludedInstVars includes: iv ].
	allInstanceVariableNames := self class allInstVarNames.
	stonWriter
		writeObject: self
		streamMap: [ :dictionary | 
			instanceVariableNames
				do: [ :each | 
					(self instVarAt: (allInstanceVariableNames indexOf: each asSymbol))
						ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
						ifNil: [ 
							self stonShouldWriteNilInstVars
								ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ]
]

{ #category : 'accessing' }
RwSpecification >> version [

	^ nil
]
