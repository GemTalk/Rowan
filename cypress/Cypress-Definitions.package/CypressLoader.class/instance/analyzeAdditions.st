"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.
