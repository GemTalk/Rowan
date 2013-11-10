"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeKnownRepositories

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | self repositoryOn: each]