"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
diffPackages: beforePackages with: afterPackages
	"Compare two PackageSets, answering a PackageSetModification"

	| beforeDefinition afterDefinition |
	beforeDefinition := beforePackages asDefinition.
	afterDefinition := afterPackages asDefinition.
	^afterDefinition compareAgainstBase: beforeDefinition