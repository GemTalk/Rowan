"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testDetectingChangedImagePackage

	| old repo repoDigest newClass |
	self testAttachingNewCypressRepositoryToUnknownImagePackages.
	old := currentManager packageInformationNamed: 'X-Y'.
	repo := old repositories any.
	repoDigest := old digestFor: repo url.

	newClass := self addClassNamed: 'XYClass' under: Object to: UserGlobals inCatgeory: 'X-Y'.
	self addMethodNamed: 'stub' body: '' toClass: newClass inCategory: 'dummy'.

	self
		assert: old repositories size = 1
			description: 'There should have been only one repository for the package';
		assert: repoDigest
			equals: nil;
		assert: old imageDigest
			equals: nil;
		assert: (old updateDigestsFromImage; imageDigest) notNil
			description: 'After adding code to the package, the digest should be non-nil'
