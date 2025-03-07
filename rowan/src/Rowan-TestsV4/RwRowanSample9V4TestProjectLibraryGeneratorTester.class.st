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
RwRowanSample9V4TestProjectLibraryGeneratorTester >> projectReadmeIntro [
	"Intro in README -- change this to reflect that this is for V4.0 not V3.0 or V2.0 and that it is a v4 variant of RowanSample9V4"
	
	^ 'Sample project that is being used to develop the **final** version of the V4.0 project handling API.'
]

{ #category : 'accessing' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> testCase [
	^testCase
]

{ #category : 'accessing' }
RwRowanSample9V4TestProjectLibraryGeneratorTester >> testCase: object [
	testCase := object
]
