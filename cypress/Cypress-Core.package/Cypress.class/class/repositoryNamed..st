"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
repositoryNamed: urlString
	"Answer a Repository"

	^CypRepository onUrl: (CypUrl fromString: urlString)