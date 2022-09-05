Class {
	#name : 'RwRowanSample12TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample12TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample12TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	self testCase
		deny:
			(super
				_doCommitAndPush: commitMessage
				resolvedRepository: resolvedRepository
				branchName: branchName)
]

{ #category : 'accessing' }
RwRowanSample12TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample12TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
