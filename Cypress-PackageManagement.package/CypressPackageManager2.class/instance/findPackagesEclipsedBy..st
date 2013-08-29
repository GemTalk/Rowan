Accessing - private
findPackagesEclipsedBy: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [each name beginsWith: aKnownPackageInformation name , '-']]