"
notice: This work is protected by copyright. All rights reserved.
category: comparing
"
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
