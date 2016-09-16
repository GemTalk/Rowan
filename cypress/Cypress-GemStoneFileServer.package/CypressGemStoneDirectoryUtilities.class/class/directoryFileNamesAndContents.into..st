"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
directoryFileNamesAndContents: aDirectory into: aDictionary
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

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
							[| file |
							file := GsFile openReadOnServer: filename.
							file isNil
								ifFalse: 
									[aDictionary at: filename put: file contents.
									file close]]
						ifFalse: 
							[(#('/..' '/.' '\..' '\.')
								anySatisfy: [:special | filename endsWith: special])
									ifFalse: [self directoryFileNamesAndContents: filename into: aDictionary]]]]