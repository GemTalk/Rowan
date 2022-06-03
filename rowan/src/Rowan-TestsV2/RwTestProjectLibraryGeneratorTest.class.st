Class {
	#name : 'RwTestProjectLibraryGeneratorTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwTestProjectLibraryGeneratorTest >> testLibraryGenerator [
	"run RwTestProjectLibraryGenerator to ensure that spes can be regenerated without:
		1. hitting errors
		2. changing any of the disk content"

	"skip spec_004, because it makes two changes in a row, so always commits"

	| commitComment |
	RwRowanSample9Test _clearLoadSpecSessionCache.
	(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

	commitComment := 'running testLibraryGenerator tests ... no changes expected, thus no commits expected'.
	RwTestProjectLibraryGeneratorTester new
		testCase: self;
		projectName: 'RowanSample9';
		projectUrl: 'https://git@github.com/dalehenrich/RowanSample9';
		preserveRowanSHA: true;
		primeIndexCardMap;
		preserveChangesOnGithub: false;
		genSpec_0000: commitComment;
		genSpec_0001: commitComment;
		genSpec_0002: commitComment;
		genSpec_0003: commitComment;
"		genSpec_0004: commitComment;"
		genSpec_0005: commitComment;
		genSpec_0006: commitComment;
		genSpec_0007: commitComment;
		genSpec_0008: commitComment;
		genSpec_0009: commitComment;
		genSpec_0010: commitComment;
		genSpec_0011: commitComment;
		genSpec_0012: commitComment;
		genSpec_0013: commitComment;
		genSpec_0014: commitComment;
		genSpec_0015: commitComment;
		genSpec_0016: commitComment;
		genSpec_0017: commitComment;
		genSpec_0018: commitComment;
		genSpec_0019: commitComment;
		genSpec_0020: commitComment;
		genSpec_0021: commitComment;
		genSpec_0022: commitComment;
		genSpec_0023: commitComment;
		genSpec_0024: commitComment;
		genSpec_0025: commitComment;
		genSpec_0026: commitComment;
		genSpec_0027: commitComment;
		genSpec_0028: commitComment;
		genSpec_0029: commitComment;
		genSpec_0030: commitComment;
		genSpec_0031: commitComment;
		genSpec_0032: commitComment;
		genSpec_0033: commitComment;
		genSpec_0034: commitComment;
		genSpec_0035: commitComment;
		genSpec_0036: commitComment;
		genSpec_0037: commitComment;
		genSpec_0038: commitComment;
		genSpec_0039: commitComment;
		genSpec_0040: commitComment;
		genSpec_0041: commitComment;
		genSpec_0042: commitComment;
		genSpec_0043: commitComment;
		genSpec_0044: commitComment;
		genSpec_0045: commitComment;
		genSpec_0046: commitComment;
		genSpec_0047: commitComment;
		genSpec_0048: commitComment;
		genSpec_0049: commitComment;
		genSpec_0050: commitComment;
		genSpec_0051: commitComment;
		genSpec_0052: commitComment;
		genSpec_0053: commitComment;
		genSpec_0054: commitComment;
		genSpec_0055: commitComment;
		genSpec_0056: commitComment;
		genSpec_0057: commitComment;
		genSpec_0058: commitComment;
		genSpec_0059: commitComment;
		genSpec_0064: commitComment;
		genSpec_0079: commitComment;
		genSpec_0080: commitComment;
		genSpec_0081: commitComment;
		genSpec_0082: commitComment;
		yourself
]

{ #category : 'tests' }
RwTestProjectLibraryGeneratorTest >> testRowanSample10LibraryGenerator [
	"run RwTestProjectLibraryGenerator to ensure that spes can be regenerated without:
		1. hitting errors
		2. changing any of the disk content"

	"skip spec_004, because it makes two changes in a row, so always commits"

	| commitComment |
	RwRowanSample9Test _clearLoadSpecSessionCache.
	(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample10') ensureDeleteAll.

	commitComment := 'running RowanSample10 testLibraryGenerator tests ... no changes expected, thus no commits expected'.
	RwRowanSample10TestProjectLibraryGeneratorTester new
		testCase: self;
		projectName: 'RowanSample10';
		projectUrl: 'https://git@github.com/dalehenrich/RowanSample10';
		preserveRowanSHA: true;
		primeIndexCardMap;
		preserveChangesOnGithub: false;
		genSpec_0000: commitComment;
		genSpec_0001: commitComment;
		yourself
]
