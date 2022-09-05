Class {
	#name : 'RwTestProjectLibraryIndexCard',
	#superclass : 'Object',
	#instVars : [
		'name',
		'title',
		'specName',
		'index',
		'derivedFrom',
		'comment',
		'rowanIssues',
		'gemstoneIssues',
		'rowanSHA'
	],
	#category : 'Rowan-TestsV2'
}

{ #category : 'instance creation' }
RwTestProjectLibraryIndexCard class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ STON fromStream: stream ]
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> comment [
	^comment
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> comment: object [
	comment := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> derivedFrom [
	^derivedFrom
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> derivedFrom: object [
	derivedFrom := object
]

{ #category : 'export' }
RwTestProjectLibraryIndexCard >> exportTo: directoryReference [
	self exportTo: directoryReference as: self name , '.ston'
]

{ #category : 'export' }
RwTestProjectLibraryIndexCard >> exportTo: directoryReference as: fileName [
	directoryReference / fileName
		writeStreamDo: [ :fileStream | self exportToStream: fileStream ]
]

{ #category : 'export' }
RwTestProjectLibraryIndexCard >> exportToStream: fileStream [
	STON put: self onStreamPretty: fileStream
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> gemstoneIssues [
	^gemstoneIssues
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> gemstoneIssues: object [
	gemstoneIssues := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> index [
	^index
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> index: object [
	index := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> name [
	^name
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> name: object [
	name := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> rowanIssues [
	^rowanIssues
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> rowanIssues: object [
	rowanIssues := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> rowanSHA [
	"The SHA of the Rowan commit used to create this index card"

	^rowanSHA
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> rowanSHA: object [
	rowanSHA := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> specName [
	^specName
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> specName: object [
	specName := object
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> title [
	^title
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> title: object [
	title := object
]
