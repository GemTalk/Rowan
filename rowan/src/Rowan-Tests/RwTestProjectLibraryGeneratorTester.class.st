Class {
	#name : 'RwTestProjectLibraryGeneratorTester',
	#superclass : 'RwTestProjectLibraryGenerator',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
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
RwTestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwTestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
