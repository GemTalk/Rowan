"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support-definitionBuilding
"
package2Definition

	^CypPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage2')
		classDefinitions: (Dictionary
				with: 'TestClass2' -> (self definitionOfSimpleClassNamed: 'TestClass2'))
		classExtensions: Dictionary new