Class {
	#name : 'RwProjectSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectName',
		'projectSpecPath',
		'componentsPath',
		'packagesPath',
		'projectsPath',
		'specsPath',
		'packageFormat',
		'packageConvention',
		'comment',
		'repoType',
		'loadedCommitId'
	],
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'accessing' }
RwProjectSpecificationV2 class >> label [
	^ 'project specification '
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> _repoType [
	"direct access to IV ... used by ="

	^ repoType
]

{ #category : 'private' }
RwProjectSpecificationV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	#(#'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'projectName' #'specName' #'projectSpecPath')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	^ true
]

{ #category : 'comparing' }
RwProjectSpecificationV2 >> = anObject [
	| lazyEqual |
	^ self specName = anObject specName
		and: [ 
			self projectName = anObject projectName
				and: [ 
					self componentsPath = anObject componentsPath
						and: [ 
							self packageFormat = anObject packageFormat
								and: [ 
									self packageConvention = anObject packageConvention
										and: [ 
											self packagesPath = anObject packagesPath
												and: [ 
													self projectsPath = anObject projectsPath
														and: [ 
															self specsPath = anObject specsPath
																and: [ 
																	lazyEqual := self _repoType = anObject _repoType
																		or: [ self repoType = anObject repoType ].
																	lazyEqual
																		and: [ 
																			self comment = anObject comment
																				and: [ self loadedCommitId = anObject loadedCommitId ] ] ] ] ] ] ] ] ] ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> componentsPath [

	^ componentsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'exporting' }
RwProjectSpecificationV2 >> exportTo: directoryReference [
	| filePath |
	filePath := self projectSpecPath
		ifNil: [ directoryReference / self specName , 'ston' ]
		ifNotNil: [ :path | directoryReference / path / self specName , 'ston' ].
	filePath
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
]

{ #category : 'comparing' }
RwProjectSpecificationV2 >> hash [
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self componentsPath hash.
	hashValue := hashValue bitXor: self packagesPath hash.
	hashValue := hashValue bitXor: self projectsPath hash.
	hashValue := hashValue bitXor: self specsPath hash.
	hashValue := hashValue bitXor: self _repoType hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self packageFormat hash.
	hashValue := hashValue bitXor: self packageConvention hash.
	hashValue := hashValue bitXor: self loadedCommitId hash.
	^ hashValue
]

{ #category : 'initialization' }
RwProjectSpecificationV2 >> initialize [
	super initialize.
	componentsPath := 'rowan/components'.
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
	projectSpecPath := 'rowan'.
	specName := 'project'.
	comment := ''
]

{ #category : 'initialization' }
RwProjectSpecificationV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	projectName := loadedCommitId := repoType := nil
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> loadedCommitId [
	^ loadedCommitId ifNil: [ '' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> loadedCommitId: aCommitId [
	loadedCommitId := aCommitId
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageConvention [
	^ packageConvention ifNil: [ 'RowanHybrid' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageConvention: aString [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	packageConvention := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormat [
	^ self packageFormatIfAbsent: [ 'tonel' ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormat: aString [
	(#('tonel' 'filetree') includes: aString)
		ifFalse: [ 
			self
				error:
					'Unknown package format ' , aString printString
						, '. Should be one of: tonel, or filetree' ].
	packageFormat := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packageFormatIfAbsent: absentBlock [
	^ packageFormat ifNil: absentBlock
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packagesPath [

	^ packagesPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> packagesPath: aString [
	packagesPath := aString
]

{ #category : 'printing' }
RwProjectSpecificationV2 >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: (self specName ifNil: ['nil'])
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectName [

	^ projectName
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsPath [

	^ projectsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectsPath: aString [
	projectsPath := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectSpecPath [

	^ projectSpecPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectSpecPath: aStringOrNil [
	"nil value indicates projectSpec file is in repository root directory"

	projectSpecPath := aStringOrNil
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectVersion [
	^ nil
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> projectVersion: aStringOrVersion [
	"noop for now"
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repoType [
	^ repoType ifNil: [ repoType := #disk ]
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> repoType: aSymbol [
	"#disk, #git or #none"

	(#(#'disk' #'git' #'none') includes: aSymbol asSymbol)
		ifFalse: [ self error: 'Unknown repo type ' , aSymbol asSymbol printString ].
	^ repoType := aSymbol asSymbol
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specName [

	^ specName
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specsPath [

	^ specsPath
]

{ #category : 'accessing' }
RwProjectSpecificationV2 >> specsPath: aString [
	specsPath := aString
]

{ #category : 'v2 vs v3' }
RwProjectSpecificationV2 >> useV3ComponentClasses [

	^ false
]
