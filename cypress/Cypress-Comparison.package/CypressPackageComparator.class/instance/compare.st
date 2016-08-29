"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
