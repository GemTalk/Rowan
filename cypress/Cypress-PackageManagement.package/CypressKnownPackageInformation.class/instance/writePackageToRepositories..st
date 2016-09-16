"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Writing
"
writePackageToRepositories: someRepositories

	| packageStructure |
	packageStructure := self packageStructure.
	^someRepositories
		do: [:each | each writePackageStructure: packageStructure]