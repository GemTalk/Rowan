"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
deleteAll: aDirectory rejecting: rejectBlock
	"Delete all the files and directories under the named directory.
       Reject file and directores in aDirectory that are rejected by rejectBlock.
       The rejectBlock is not used recursively.
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
						ifTrue: 
							[(rejectBlock value: filename)
								ifFalse: 
									[(rejectBlock value: filename) ifFalse: [GsFile removeServerFile: filename]]]
						ifFalse: 
							[(self endsWithSpecial: filename)
								ifFalse: 
									[(rejectBlock value: filename)
										ifFalse: 
											[self deleteAll: filename.
											GsFile removeServerDirectory: filename]]]]]
