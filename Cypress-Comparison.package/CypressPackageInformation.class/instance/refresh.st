"
notice: This work is protected by copyright. All rights reserved.
category: updating
"
refresh

	self isKnown ifFalse: [^self].
	self
		updateImageDefinitions;
		updateSavedDefinitions;
		updateChangesCount.
