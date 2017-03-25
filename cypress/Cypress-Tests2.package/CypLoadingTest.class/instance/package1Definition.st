"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support-definitionBuilding
"
package1Definition

	^CypPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage1')
		classDefinitions: (Dictionary
				with: 'TestClass1' -> (self definitionOfSimpleClassNamed: 'TestClass1'))
		classExtensions: Dictionary new