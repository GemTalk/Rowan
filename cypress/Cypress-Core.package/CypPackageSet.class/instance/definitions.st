"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
definitions
	"Answer a dictionary of package definitions, keyed by name."

	| result |
	result := Dictionary new.
	packages
		do: [:packageRef | result at: packageRef name put: packageRef asDefinition].
	^result