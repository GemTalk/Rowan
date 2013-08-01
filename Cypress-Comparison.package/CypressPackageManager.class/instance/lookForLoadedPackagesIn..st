updating
lookForLoadedPackagesIn: aDirectory
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	| packageDirectories |
	packageDirectories := GsFile contentsOfDirectory: aDirectory , '*.package' onClient: false.
	(self packageInformationList
		select: [:each | packageDirectories includes: aDirectory , each name , '.package'])
			do: 
				[:each |
				each updateKnownPackageSavedLocation: aDirectory
					or: [self error: 'invalid directory path']].
	self saveKnownPackages.
	^nil