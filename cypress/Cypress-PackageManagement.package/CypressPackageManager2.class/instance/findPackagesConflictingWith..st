"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing - private
"
findPackagesConflictingWith: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name and: 
					[(aKnownPackageInformation name indexOfSubCollection: each name , '-') = 1]]
