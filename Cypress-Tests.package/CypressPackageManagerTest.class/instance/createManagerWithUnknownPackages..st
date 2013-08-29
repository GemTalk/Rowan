private
createManagerWithUnknownPackages: someNames

	| manager |
	manager := self createEmptyManager.
	someNames do: [:each | manager addUnknownPackageNamed: each].
	^manager
