load support
resolvePackageNames: visitedConfigurationNames

	| thePackageNames |
	(visitedConfigurationNames includes: self name)
		ifTrue: [ ^ #() ].
	visitedConfigurationNames add: self name.
	thePackageNames := self packageNames copy.
	self _configurations
		do: [ :config | thePackageNames addAll: (config resolvePackageNames: visitedConfigurationNames) ].
	^ thePackageNames