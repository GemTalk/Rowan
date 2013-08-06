"
notice: This work is protected by copyright. All rights reserved.
category: updating
"
lookForLoadedPackagesIn: aDirectory
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	| repo |
	repo := knownRepositories
				at: aDirectory
				ifAbsent: [CypressFileSystemRepository on: aDirectory].
	self lookForLoadedPackagesInRepository: repo.
	^nil