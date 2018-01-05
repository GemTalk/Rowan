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

{ #category : 'testing' }
RwLoadSpecification >> canCommit [
  ^ self repositoryUrl notNil and: [ self repoSpec canCommit ]
]

{ #category : 'accessing' }
RwLoadSpecification >> comment [

   ^comment

]

{ #category : 'accessing' }
RwLoadSpecification >> comment: anObject [

   comment := anObject

]

{ #category : 'actions' }
RwLoadSpecification >> commitForTool: aRwTool message: messageString [
  ^ self repoSpec commitForTool: aRwTool message: messageString
]

{ #category : 'actions' }
RwLoadSpecification >> commitLogForTool: aRwTool limit: logLimit [
  ^ self repoSpec commitLogForTool: aRwTool limit: logLimit
]

{ #category : 'exporting' }
RwLoadSpecification >> export [
  | url exportUrl exportSpec |
  self repositoryUrl
    ifNil: [ self error: 'exportToUrl: must be if repositoryUrl has not been set' ].
  url := CypUrl fromString: self repositoryUrl.
  exportUrl := 'file:'
    ,
      (ServerFileDirectory on: url pathForDirectory) containingDirectory fullName
    , '/' , self specsPath , '/'.
  exportSpec := self copy.
  exportSpec initializeForHttpsDownload.	"clear fields that only apply to local image"
  ^ exportSpec exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwLoadSpecification >> exportToUrl: fileUrl [
  | url |
  url := CypUrl fromString: fileUrl.
  url schemeName = 'file'
    ifTrue: [ 
      CypressFileUtilities current
        writeStreamFor: self specName , '.ston'
        in: url pathForDirectory
        do: [ :stream | 
          | string |
          string := STON toStringPretty: self.
          stream nextPutAll: string.
          ^ self ] ].
  ^ nil	"otherwise a noop"
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
  repositoryUrl := nil.
  repoSpec initializeForHttpsDownload
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
