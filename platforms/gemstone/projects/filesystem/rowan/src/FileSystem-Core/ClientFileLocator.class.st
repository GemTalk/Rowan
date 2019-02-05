Class {
	#name : 'ClientFileLocator',
	#superclass : 'FileLocator',
	#category : 'FileSystem-Core'
}

{ #category : 'accessing' }
ClientFileLocator class >> resolver [

	^ SessionTemps current at: #FileLocator_ClientResolver ifAbsentPut: [
		InteractiveResolver new
			addResolver: ClientResolver forCurrentPlatform;
			yourself ]
]

{ #category : 'streams' }
ClientFileLocator >> streamFor: aFileSpec [
	aFileSpec store: FileSystem clientDisk store.
	^ super streamFor: aFileSpec
]
