"
| commitComment |
RwRowanSample10Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample10') ensureDeleteAll.

commitComment := ''.
RwRowanSample10TestProjectLibraryGenerator new
	projectName: 'RowanSample10';
	projectUrl: 'git@github.com:dalehenrich/RowanSample10.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	genSpec_0002: commitComment;
	genSpec_0003: commitComment;
	genSpec_0004: commitComment;
	genSpec_0005: commitComment;
	yourself
"
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
RwRowanSample10V3TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample10V3TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
