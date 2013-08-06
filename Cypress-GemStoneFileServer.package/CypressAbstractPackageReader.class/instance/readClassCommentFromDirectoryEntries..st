"
notice: This work is protected by copyright. All rights reserved.
category: reading
"
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
