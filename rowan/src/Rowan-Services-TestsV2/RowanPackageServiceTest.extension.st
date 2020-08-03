Extension { #name : 'RowanPackageServiceTest' }

{ #category : '*rowan-services-testsv2' }
RowanPackageServiceTest >> test_packageWasDeleted [
	"NOTE - use commit/abort in tests carefully. 
	Can cause hard-to-diagnose problems later 
	in test runs"

	| packageService projectDef |
	self
		jadeiteIssueTested: #'issue284'
		withTitle:
			'(3.0.49 and 3.0.50) project browser not updated properly on reload of project'.
	projectDef := self defaultProjectDefinition.
	[ 
	System commitTransaction.	"commit project but not package"
	projectDef
		packageNamed: self servicesTestPackageName
		ifAbsent: [ 
			projectDef
				addPackageNamed: self servicesTestPackageName
				toComponentNamed: self servicesTestComponentName ].
	projectDef load.
	packageService := RowanPackageService
		forPackageNamed: self servicesTestPackageName.
	self deny: packageService wasDeleted.
	System abortTransaction.
	self assert: packageService wasDeleted ]
		ensure: [ 
			self unloadServicesTestProject.
			System commitTransaction ]
]
