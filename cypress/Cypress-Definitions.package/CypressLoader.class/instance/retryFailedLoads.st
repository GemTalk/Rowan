"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
retryFailedLoads
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self additions intersection: failed);
		unloadRemovals: (self removals intersection: failed).
