"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
repositoryOn: aDirectory

	^knownRepositories
		at: aDirectory
		ifAbsentPut: [CypressFileSystemRepository on: aDirectory].
