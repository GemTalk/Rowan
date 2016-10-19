"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: expected results
"
fileTreeFormatXYPackageDirectoryStructure

	^(Dictionary new)
		"at: '/tmp/CypressTestRepo/properties.json'
			put: '{
	""alias"" : ""CypressTestRepo"",
	""_cypress_copyright"" : ""This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved."",
	""_gs_fileout"" : ""false"",
	""_gs_format"" : ""FileTree"",
	""_gs_strict"" : ""false"" }';"
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'
			put: '*X-Y-testing
isXY   ^false';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'
			put: '{
	"name" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{
	 }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'
			put: 'dummy
stub   ^nil';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/properties.ston'
			put: '{
	"category" : "X-Y",
	"classinstvars" : [
		 ],
	"classvars" : [
		 ],
	"instvars" : [
		 ],
	"name" : "XYClass",
	"pools" : [
		 ],
	"super" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/README.md' put: '';
		yourself