utilities
deleteAll: aDirectory
	"Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: [GsFile removeServerFile: filename]
						ifFalse: 
							[(#('/..' '/.' '\..' '\.')
								anySatisfy: [:special | filename endsWith: special])
									ifFalse: [self deleteAll: filename]]]]