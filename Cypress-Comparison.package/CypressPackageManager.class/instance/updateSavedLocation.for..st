updating
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update package information to reflect the path where the package should be saved."

	aCypressPackageInformation updateKnownPackageSavedLocation: aDirectory
		or: [self error: 'invalid directory path'].
	self saveKnownPackages.
	^nil