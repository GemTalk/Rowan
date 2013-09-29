"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testSavingChangedImagePackageToFileTreeReadOnlyRepository

	| repo groups old new repoDigest newClass results |
	currentManager := self createManagerWithUnknownPackages: #('X-Y').
	repo := self createFileTreeReadOnlyTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	currentManager assignRepository: repo to: old.
	old := currentManager packageInformationNamed: 'X-Y'.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '^nil' toClass: newClass inCategory: 'dummy'.
	self addMethodNamed: 'isXY' body: '^false' toClass: Object inCategory: '*X-Y-testing'.

	currentManager writeChangesToAllRepositoriesFor: old.

	results := CypressFileUtilities current directoryFileNamesAndContents: repo directoryPath.
	self assert: results equals: self readOnlyFileTreeFormatXYPackageDirectoryStructure.
