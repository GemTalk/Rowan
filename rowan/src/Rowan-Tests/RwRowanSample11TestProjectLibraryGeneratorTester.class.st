Class {
	#name : 'RwRowanSample11TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample11TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample11TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	self testCase
		deny:
			(super
				_doCommitAndPush: commitMessage
				resolvedRepository: resolvedRepository
				branchName: branchName)
]

{ #category : 'accessing' }
RwRowanSample11TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample11TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
