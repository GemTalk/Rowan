Class {
	#name : 'RwLoadSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'symbolDictName',
		'repositoryUrl',
		'userId',
		'methodEnv'
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
