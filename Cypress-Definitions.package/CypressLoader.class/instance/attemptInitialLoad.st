"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
