class reading
readClassFile: file inPackage: packageName

	| fileReference |
	fileReference := file asFileReference.
	fileReference readStreamDo: [:fileStream |
		| definitions stream |
		stream := ZnBufferedReadStream on: fileStream. "wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
		stream sizeBuffer: fileReference size. "part of workaround for GemTalk/FileSystemGs#9"
		definitions := (TonelParser on: stream forReader: self) start.
		self validateClassCategory: currentClassDefinition forPackageNamed: packageName.
		((definitions at: 2) at: 1) do: [:mDef |
			currentClassDefinition addClassMethodDefinition: mDef ].
		((definitions at: 2) at: 2) do: [:mDef |
			currentClassDefinition addInstanceMethodDefinition: mDef ].
		(currentProjectDefinition packageNamed: packageName)
			addClassDefinition: currentClassDefinition ].