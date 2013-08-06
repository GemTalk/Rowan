"
notice: This work is protected by copyright. All rights reserved.
category: initializing
"
initializeKnownRepositories

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | knownRepositories at: each put: (CypressFileSystemRepository on: each)]