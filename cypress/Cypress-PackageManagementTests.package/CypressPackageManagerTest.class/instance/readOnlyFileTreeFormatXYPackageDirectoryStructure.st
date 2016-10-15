"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: expected results
"
readOnlyFileTreeFormatXYPackageDirectoryStructure
	"We cannot commit to a true FileTree repository, so there should be no files created."

	^(Dictionary new)
		at: '/tmp/CypressTestRepo/properties.json'
			put: '{
	"alias" : "CypressTestRepo",
	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",
	"_gs_fileout" : "false",
	"_gs_format" : "FileTree",
	"_gs_strict" : "true" }';
		yourself