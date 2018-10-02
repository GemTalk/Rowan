"
I resolve origins that are related to the currently running Smalltalk system, using primitives provided by the VM. 
"
Class {
	#name : 'SystemResolver',
	#superclass : 'FileSystemResolver',
	#classVars : [
		'UserLocalDirectory'
	],
	#category : 'FileSystem-Core-Base-Resolver'
}

{ #category : 'accessing' }
SystemResolver class >> defaultLocalDirectoryName [
	^ 'pharo-local'
]

{ #category : 'accessing' }
SystemResolver class >> userLocalDirectory [
	^ UserLocalDirectory ifNil: [ 
		(FileLocator imageDirectory / self defaultLocalDirectoryName) resolve  ]

]

{ #category : 'accessing' }
SystemResolver class >> userLocalDirectory: aFileReference [
	UserLocalDirectory := aFileReference
]

{ #category : 'private' }
SystemResolver >> _extent: extentIndex [
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	| extentNames extentName |
	extentNames := System stoneConfigurationReport at: #DBF_EXTENT_NAMES.
	extentNames _isArray
		ifTrue: [  
			extentIndex > extentNames size ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (', extentNames size printString, ')'  ].
			extentName := extentNames ]
		ifFalse: [ 
			extentIndex ~= 1 ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (1)'  ].
			extentName := extentNames ].
	^ self  resolveString: extentName
]

{ #category : 'origins' }
SystemResolver >> dbfScratchDir [
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self  resolveString: (System stoneConfigurationReport at: #DBF_SCRATCH_DIR)
]

{ #category : 'origins' }
SystemResolver >> extent1 [

	^ self _extent: 1
]

{ #category : 'origins' }
SystemResolver >> extent1Directory [

	^ self extent1 parent
]

{ #category : 'origins' }
SystemResolver >> gemLogDirectory [
	"Answer the path to the gem log directory"

	^ self  resolveString: (System gemLogPath)
]

{ #category : 'origins' }
SystemResolver >> image [

	^ self dbfScratchDir
]

{ #category : 'origins' }
SystemResolver >> imageDirectory [

	^ self dbfScratchDir
]

{ #category : 'resolving' }
SystemResolver >> supportedOrigins [
	^ #(image imageDirectory extent1 extent1Directory tranlog dbfScratchDir workingDirectory)
]

{ #category : 'origins' }
SystemResolver >> tranlog [
	"Answer the path to the current tranlog"

	^ self  resolveString: (System stoneConfigurationReport at: #StnCurrentTranLogNames)
]
