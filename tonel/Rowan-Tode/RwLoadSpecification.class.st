Class {
	#name : 'RwLoadSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'repositoryUrl',
		'platformSpec',
		'specsPath',
		'repoSpec',
		'repoPath',
		'comment'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwLoadSpecification >> _myUserProfile [
  ^ System myUserProfile
]

{ #category : 'accessing' }
RwLoadSpecification >> comment [

   ^comment

]

{ #category : 'accessing' }
RwLoadSpecification >> comment: anObject [

   comment := anObject

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

{ #category : 'initialization' }
RwLoadSpecification >> initialize [
  super initialize.
  comment := ''.
  specsPath := 'specs'.
  platformSpec := (Dictionary new at: 'gemstone' put: RwGemStoneSpecification new; yourself).
  repoSpec := RwGitRepositorySpecification new
]

{ #category : 'accessing' }
RwLoadSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	self subclassResponsibility: #packageOrGroupNamesToLoad:
]

{ #category : 'accessing' }
RwLoadSpecification >> platformSpec [

   ^platformSpec

]

{ #category : 'accessing' }
RwLoadSpecification >> platformSpec: anObject [

   platformSpec := anObject

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
RwLoadSpecification >> repoSpec [

   ^repoSpec

]

{ #category : 'accessing' }
RwLoadSpecification >> repoSpec: anObject [

   repoSpec := anObject

]

{ #category : 'accessing' }
RwLoadSpecification >> specsPath [

   ^specsPath

]

{ #category : 'accessing' }
RwLoadSpecification >> specsPath: anObject [

   specsPath := anObject

]
