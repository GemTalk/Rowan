Class {
	#name : 'RwTestProjectLibraryGeneratorTesterV4',
	#superclass : 'RwTestProjectLibraryGeneratorTesterV3',
	#category : 'Rowan-TestsV4'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorTesterV4 >> _createNewProject: aString [
	^ Rowan newV4ProjectNamed: aString
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorTesterV4 >> primaryProjectName [
	^ 'RowanSample9V4'
]
