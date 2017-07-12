"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | self string: entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]