"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPropertiesFile: fileName
	"Expect 'properties.ston' for Cypress, but permit 'properties.json' in which case we assume
	 the format should be FileTree.

	 Supported properties are:
		_cypress_copyright	- the (optional, default) copyright notice for the whole repository
		_gs_format			- optional, determines which format will be used for writing packages (and reading, but ...)
							- either Cypress or FileTree (case-insensitive)
		_gs_strict			- optional, determines whether the reader strictly enforces the format
							- either true or false (case-insensitive)
		_gs_fileout			- optional, determines whether to also produce a *.gs fileout when writing a package
							- either true or false (case-insensitive)
	"

	fileName = '.filetree' ifTrue: [^self initializeForFileTreeRepository].
	self fileUtils
		readStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties := CypressJsonParser parseStream: fileStream]