"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"If the stream begins with a double quote, process it on the assumption it is a Cypress-format method.
	 Otherwise, treat it as a FileTree-format method."

	fileStream peek = $"
		ifTrue: 
			[self
				readCypressFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
		ifFalse: 
			[self
				readFileTreeFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
