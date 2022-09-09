Class {
	#name : 'RwRowanSample10V3TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample10V3TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample10V3TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	self testCase
		deny:
			(super
				_doCommitAndPush: commitMessage
				resolvedRepository: resolvedRepository
				branchName: branchName)
]

{ #category : 'accessing' }
RwRowanSample10V3TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample10V3TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
