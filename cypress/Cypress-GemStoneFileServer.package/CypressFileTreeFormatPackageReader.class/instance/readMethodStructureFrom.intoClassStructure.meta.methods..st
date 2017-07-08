"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!"

	self
		readFileTreeFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods
