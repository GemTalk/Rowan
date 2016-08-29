"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
