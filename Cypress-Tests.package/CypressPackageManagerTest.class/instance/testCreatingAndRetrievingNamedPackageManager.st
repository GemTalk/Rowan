tests
testCreatingAndRetrievingNamedPackageManager

	| original retrieved key savedPackageManagers |
	key := DateAndTime now printString.
	original := CypressPackageManager2 create.
	savedPackageManagers := CypressPackageManager2 savedPackageManagers copy.
	
	[original saveAs: key.
	retrieved := CypressPackageManager2 named: key.
	self assert: original == retrieved
		description: 'a retrieved instance should be identical to the original']
			ensure: [CypressPackageManager2 removePackageManagerSavedAs: key].
	self assert: CypressPackageManager2 savedPackageManagers
		equals: savedPackageManagers