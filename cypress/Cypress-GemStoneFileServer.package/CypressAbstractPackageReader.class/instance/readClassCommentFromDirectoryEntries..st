"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
