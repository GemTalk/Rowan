loading
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
