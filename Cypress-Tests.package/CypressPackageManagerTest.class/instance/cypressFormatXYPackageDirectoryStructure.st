expected results
cypressFormatXYPackageDirectoryStructure

	^(Dictionary new)
		at: '/tmp/CypressTestRepo/properties.ston'
			put: '{
	"alias" : "CypressTestRepo",
	"_cypress_copyright" : "This%20work%20is%20protected%20by%20copyright.%20All%20rights%20reserved.",
	"_gs_fileout" : "false",
	"_gs_format" : "Cypress",
	"_gs_strict" : "true" }';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/instance/isXY.st'
			put: '"
notice: This work is protected by copyright. All rights reserved.
category: *X-Y-testing
"
isXY   ^false';
		at: '/tmp/CypressTestRepo/X-Y.package/Object.extension/properties.ston'
			put: '{
	"name" : "Object" }';
		at: '/tmp/CypressTestRepo/X-Y.package/properties.ston' put: '{
	 }';
		at: '/tmp/CypressTestRepo/X-Y.package/XYClass.class/instance/stub.st'
			put: '"
notice: This work is protected by copyright. All rights reserved.
category: dummy
"
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