"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating - digests
"
updateDigestsFromAllRepositories

	self repositories do: [:each | self updateDigestsFromRepository: each]