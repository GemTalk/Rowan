"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
packageNamed: aName
	"Answer a PackageReference, if the given package name is unique in all registered repositories. If the name resolves 0 or >=2 times, error."

	| token resolutions |
	token := 'notFound'.
	resolutions := (self knownRepositories
				collect: [:repo | repo packageNamed: aName ifAbsent: [token]])
					select: [:each | each ~~ token].
	resolutions isEmpty ifTrue: [self error: 'Named package not found'].
	resolutions size > 1
		ifTrue: [self error: 'Package name is not unique in registered repositories.'].
	^resolutions any