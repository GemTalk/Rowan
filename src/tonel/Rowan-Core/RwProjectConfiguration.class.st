Class {
	#name : 'RwProjectConfiguration',
	#superclass : 'Object',
	#instVars : [
		'name',
		'projectName',
		'configNames',
		'comment'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwProjectConfiguration class >> newNamed: aName for: projectName [

	^ self new
		name: aName;
		projectName: projectName;
		yourself
]

{ #category : 'private' }
RwProjectConfiguration >> _configsPath [

	^ self _specification configsPath
]

{ #category : 'private' }
RwProjectConfiguration >> _loadedProject [

	^ Rowan image loadedProjectNamed: self projectName
]

{ #category : 'private' }
RwProjectConfiguration >> _repositoryRootPath [

	^ self _specification repositoryRootPath
]

{ #category : 'private' }
RwProjectConfiguration >> _repositoryUrl [

	^ self _specification repositoryUrl
]

{ #category : 'private' }
RwProjectConfiguration >> _specification [

	^ self _loadedProject specification
]

{ #category : 'configs' }
RwProjectConfiguration >> addConfig: configName [

	self configNames add: configName
]

{ #category : 'accessing' }
RwProjectConfiguration >> comment [

   ^comment

]

{ #category : 'accessing' }
RwProjectConfiguration >> comment: anObject [

   comment := anObject

]

{ #category : 'accessing' }
RwProjectConfiguration >> configNames [

	^ configNames ifNil: [ configNames := Set new ]
]

{ #category : 'accessing' }
RwProjectConfiguration >> configNames: anObject [

   configNames := anObject

]

{ #category : 'exporting' }
RwProjectConfiguration >> export [

	| url exportUrl exportPath export |
	self _repositoryUrl
		ifNil: [ self error: 'exportToUrl: must be used if repositoryUrl has not been set' ].
	url := RwUrl fromString: self _repositoryUrl.
	exportPath := self _repositoryRootPath , '/' , self _configsPath , '/'.
	Rowan fileUtilities ensureDirectoryExists: exportPath.
	exportUrl := 'file:' , exportPath.

	export := self copy.
	export initializeForExport.
	^ export exportToUrl: exportUrl
]

{ #category : 'exporting' }
RwProjectConfiguration >> exportToUrl: fileUrl [

	| url |
	url := RwUrl fromString: fileUrl.
	url schemeName = 'file'
		ifTrue: [ 
			Rowan fileUtilities
				writeStreamFor: self name , '.ston'
				in: url pathForDirectory
				do: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	^ nil	"otherwise a noop"
]

{ #category : 'initialization' }
RwProjectConfiguration >> initializeForExport [
  "if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

  projectName := nil
]

{ #category : 'accessing' }
RwProjectConfiguration >> name [

   ^name

]

{ #category : 'initialization' }
RwProjectConfiguration >> name: anObject [

   name := anObject

]

{ #category : 'accessing' }
RwProjectConfiguration >> projectName [

   ^projectName

]

{ #category : 'initialization' }
RwProjectConfiguration >> projectName: anObject [

   projectName := anObject

]

{ #category : 'configs' }
RwProjectConfiguration >> removeConfig: configName [

	self configNames remove: configName ivAbsent: [  ]
]
