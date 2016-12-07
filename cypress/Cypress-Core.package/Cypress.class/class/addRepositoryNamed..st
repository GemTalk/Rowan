"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
addRepositoryNamed: urlString
	"Register a repository.
	Note that duplicate registrations are ignored, since knownRepositories is a Set"

	knownRepositories
		add: (CypRepository onUrl: (CypUrl fromString: urlString))