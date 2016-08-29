"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createManagerWithUnknownPackages: someNames

	| manager |
	manager := self createEmptyManager.
	someNames do: [:each | manager addUnknownPackageNamed: each].
	^manager
