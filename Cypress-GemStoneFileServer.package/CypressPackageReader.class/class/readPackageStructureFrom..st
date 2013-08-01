instance creation
readPackageStructureFrom: aPackageDirectory

	^(self new)
		packageDirectory: aPackageDirectory;
		read;
		yourself