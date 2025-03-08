Class {
	#name : 'RwRowanSample9V4TestProjectLibraryGeneratorTester',
	#superclass : 'RwTestProjectLibraryGeneratorV4',
	#instVars : [
		'testCase'
	],
	#category : 'Rowan-TestsV4'
}

{ #category : 'private' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> _doCommitAndPush: commitMessage resolvedRepository: resolvedRepository branchName: branchName [
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

{ #category : 'private' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> primaryProjectName [
	^ 'RowanSample9V4'
]

{ #category : 'accessing' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
