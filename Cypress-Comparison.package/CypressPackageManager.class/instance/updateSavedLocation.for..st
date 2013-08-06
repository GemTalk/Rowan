updating
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	| repo |
	repo := knownRepositories
				at: aDirectory
				ifAbsent: [CypressFileSystemRepository on: aDirectory].
	aCypressPackageInformation updateKnownPackageRepository: repo.
	self saveKnownPackages.
	^nil