Accessing - private
findPackagesConflictingWith: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [aKnownPackageInformation name beginsWith: each name , '-']]
