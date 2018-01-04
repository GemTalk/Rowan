Class {
	#name : 'RwLoadSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'symbolDictName',
		'repositoryUrl',
		'userId',
		'methodEnv',
		'gitCheckout',
		'gitRemote',
		'repoPath'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwLoadSpecification >> _myUserProfile [
  ^ System myUserProfile
]

{ #category : 'exporting' }
RwLoadSpecification >> exportToUrl: specNameOrUrl [

	| url |
  	url := specNameOrUrl asUrl.
	url schemeName isNil 
		ifTrue: [ "noop" ^nil].
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

{ #category : 'accessing' }
RwLoadSpecification >> gitCheckout [

   ^gitCheckout

]

{ #category : 'accessing' }
RwLoadSpecification >> gitCheckout: anObject [

   gitCheckout := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> gitRemote [

   ^gitRemote

]

{ #category : 'accessing' }
RwLoadSpecification >> gitRemote: anObject [

   gitRemote := anObject

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
RwLoadSpecification >> repoPath [

   ^repoPath

]

{ #category : 'accessing' }
RwLoadSpecification >> repoPath: anObject [

   repoPath := anObject

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
