Class {
	#name : 'RwTestProjectLibraryGeneratorTesterV3',
	#superclass : 'RwTestProjectLibraryGeneratorTester',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorTesterV3 >> _createNewProject: aString [
	^ Rowan newProjectNamed: aString
]

{ #category : 'private' }
RwTestProjectLibraryGeneratorTesterV3 >> primaryProjectName [
	^ 'RowanSample9V3'
]
