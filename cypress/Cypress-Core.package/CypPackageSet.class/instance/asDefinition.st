"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
asDefinition
	"Read the full definitions of each of my PackageReferences from its repository, and answer the resulting PackageSetDefinition. "

	| result |
	result := CypPackageSetDefinition new.
	packages do: [:packageRef | result addPackage: packageRef asDefinition].
	^result