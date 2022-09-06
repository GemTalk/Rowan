"
| commitComment |
RwRowanSample9V2Test _clearLoadSpecSessionCache.
(RwAbstractV2Test _testRowanProjectsSandbox / 'RowanSample9V2') ensureDeleteAll.

commitComment := ''.
RwTestProjectLibraryGeneratorV2 new
	projectName: 'RowanSample9V2';
	projectUrl: 'git@github.com:dalehenrich/RowanSample9V2.git';
	preserveRowanSHA: false;
	primeIndexCardMap;
	preserveChangesOnGithub: false;
	genSpec_0000: commitComment;
	genSpec_0001: commitComment;
	genSpec_0002: commitComment;
	genSpec_0003: commitComment;
	genSpec_0004: commitComment;
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
	genSpec_0060: commitComment;
	genSpec_0061: commitComment;
	genSpec_0062: commitComment;
	genSpec_0063: commitComment;
	genSpec_0064: commitComment;
	genSpec_0079: commitComment;
	genSpec_0080: commitComment;
	genSpec_0081: commitComment;
	yourself
"
Class {
	#name : 'RwTestProjectLibraryGeneratorV2',
	#superclass : 'RwTestProjectLibraryGenerator',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwTestProjectLibraryGeneratorV2 >> primaryProjectName [
	^ 'RowanSample9V2'
]
