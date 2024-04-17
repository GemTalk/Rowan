Class {
	#name : 'RwRowanSample9V3TestProjectLibraryGeneratorTester',
	#superclass : 'RwTestProjectLibraryGeneratorV3',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample9V3TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
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
RwRowanSample9V3TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample9V3TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
