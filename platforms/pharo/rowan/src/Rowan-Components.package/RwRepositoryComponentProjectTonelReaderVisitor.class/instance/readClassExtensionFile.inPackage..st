class reading
readClassExtensionFile: file inPackage: packageName

	file readStreamDo: [:fileStream |
		| definitions stream |
		stream := ZnBufferedReadStream on: fileStream. "wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
		stream sizeBuffer: 20000. "part of workaround for GemTalk/FileSystemGs#9"
		definitions := (TonelParser on: stream forReader: self) start.
		((definitions at: 2) at: 1) do: [:mDef |
			currentClassExtension addClassMethodDefinition: mDef ].
		((definitions at: 2) at: 2) do: [:mDef |
			currentClassExtension addInstanceMethodDefinition: mDef ] ].