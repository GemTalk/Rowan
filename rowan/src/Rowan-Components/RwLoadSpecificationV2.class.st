Class {
	#name : 'RwLoadSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectAlias',
		'projectName',
		'comment',
		'componentNames',
		'groupNames',
		'gitUrl',
		'diskUrl',
		'mercurialUrl',
		'svnUrl',
		'revision',
		'projectSpecFile'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwLoadSpecificationV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| repoUrls |
	#(#'componentNames' #'groupNames' #'specName' #'projectSpecFile')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	repoUrls := {}.
	#(#'gitUrl' #'diskUrl' #'mercurialUrl' #'svnUrl')
		do: [ :messageName | (self perform: messageName) ifNotNil: [ repoUrls add: messageName asString ] ].
	repoUrls size > 1
		ifTrue: [ Error signal: 'Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set' ].
	(repoUrls size = 0 or: [ repoUrls = #('diskUrl') ])
		ifTrue: [ 
			self revision
				ifNotNil: [ :rev | 
					Error
						signal:
							'Invalid revision ' , rev printString
								, '. Should be nil for disk-based repository' ] ]
		ifFalse: [ 
			self gitUrl asRwUrl scheme ~= 'file'
				ifTrue: [ 
					Error
						signal:
							'The instance variable ''revision'' must be set for the'
								, (repoUrls at: 1) asString printString ] ].
	^ true
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl [
	^ diskUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> diskUrl: anUrlString [
	diskUrl := anUrlString
]

{ #category : 'exporting' }
RwLoadSpecificationV2 >> exportTo: directoryReference [
	(directoryReference / self specName, 'ston')
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl [
	^ gitUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> gitUrl: anUrlString [
	gitUrl := anUrlString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initialize [
	super initialize.
	componentNames := {}.
	groupNames := {'core'.
	'tests'}.
	projectSpecFile := 'rowan/project.ston'
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> mercurialUrl [
	^ mercurialUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> mercurialUrl: anUrlString [
	mercurialUrl := anUrlString
]

{ #category : 'copying' }
RwLoadSpecificationV2 >> postCopy [
	"don't forget to copy any compound state objects"

	super postCopy.
	componentNames := componentNames copy.
	groupNames := groupNames copy
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectAlias: aString [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	projectAlias := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectName [
	^ projectName
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectSpecFile [
	^ projectSpecFile
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectSpecFile: aString [
	projectSpecFile := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> revision [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> revision: aRevisionString [
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision := aRevisionString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName [

	^ specName ifNil: [ self projectAlias ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> svnUrl [
	^ svnUrl
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> svnUrlUrl: anUrlString [
	svnUrl := anUrlString
]
