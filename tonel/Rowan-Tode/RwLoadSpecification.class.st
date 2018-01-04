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
RwLoadSpecification >> export [

	| url exportUrl exportSpec |
	self repositoryUrl
		ifNil: [ 
			self
			error: 'exportToUrl: must be if repositoryUrl has not been set' ].
	url := self repositoryUrl asUrl.
	exportUrl := 'file:'
		, (ServerFileDirectory on: url locator) containingDirectory fullName
		, '/' , self specsPath , '/'.
	exportSpec := self copy.
	exportSpec initializeForHttpsDownload. "clear fields that only apply to local image"
	^exportSpec exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwLoadSpecification >> exportToUrl: fileUrl [

	| url |
  	url := fileUrl asUrl.
	url schemeName = 'file' 
		ifTrue: [ 
			CypressFileUtilities current 
				writeStreamFor: self specName, '.ston'
				in: url pathForDirectory 
				do: [:stream |  | string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	^nil "otherwise a noop"

]

{ #category : 'initialization' }
RwLoadSpecification >> initialize [
  super initialize.
  comment := ''.
  specsPath := 'specs'.
  platformSpec := (Dictionary new at: 'gemstone' put: RwGemStoneSpecification new; yourself).
  repoSpec := RwGitRepositorySpecification new
]

{ #category : 'initialization' }
RwLoadSpecification >> initializeForHttpsDownload [
	"if spec has been downloaded, clear out any of the fields that represent local disk state"

	super initializeForHttpsDownload.
	repositoryUrl := nil
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
