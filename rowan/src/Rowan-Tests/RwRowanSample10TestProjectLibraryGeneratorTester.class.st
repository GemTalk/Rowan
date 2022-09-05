Class {
	#name : 'RwRowanSample10TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample10TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample10TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	self testCase
		deny:
			(super
				_doCommitAndPush: commitMessage
				resolvedRepository: resolvedRepository
				branchName: branchName)
]

{ #category : 'accessing' }
RwRowanSample10TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample10TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
