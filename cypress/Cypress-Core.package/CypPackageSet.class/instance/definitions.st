"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
definitions
	"Answer a dictionary of package definitions, keyed by name."

	| result |
	result := CypPackageSetDefinition new.
	packages do: [:packageRef | result addPackage: packageRef asDefinition].
	^result