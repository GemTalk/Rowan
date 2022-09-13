Class {
	#name : 'RwRowanSample12TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample12TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample12TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
	Transcript
		cr;
		show: '-- ' , self class name;
		cr;
		show: '-- ' , branchName.
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
