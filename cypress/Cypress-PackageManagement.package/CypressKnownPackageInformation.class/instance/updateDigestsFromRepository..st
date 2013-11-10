"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating - digests
"
updateDigestsFromRepository: aRepository

	self digests at: aRepository url put: (self determineDigestFromRepository: aRepository).
