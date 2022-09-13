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
RwRowanSample10TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample10TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
