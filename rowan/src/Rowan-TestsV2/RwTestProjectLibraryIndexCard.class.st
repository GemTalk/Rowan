Class {
	#name : 'RwTestProjectLibraryIndexCard',
	#superclass : 'Object',
	#instVars : [
		'name',
		'title',
		'specName',
		'index',
		'comment',
		'rowanIssues',
		'gemstoneIssues'
	],
	#category : 'Rowan-TestsV2'
}

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> comment [
	^comment
]

{ #category : 'accessing' }
RwTestProjectLibraryIndexCard >> comment: object [
	comment := object
]

{ #category : 'export' }
RwTestProjectLibraryIndexCard >> exportTo: directoryReference [
	self exportTo: directoryReference as: self name , '.ston'
]

{ #category : 'export' }
RwTestProjectLibraryIndexCard >> exportTo: directoryReference as: fileName [
	directoryReference / fileName
		writeStreamDo: [ :fileStream | STON put: self onStreamPretty: fileStream ]
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
