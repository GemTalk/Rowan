Class {
	#name : 'RwRowanSample11TestProjectLibraryGeneratorTester',
	#superclass : 'RwRowanSample11TestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample11TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
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
RwRowanSample11TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample11TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]