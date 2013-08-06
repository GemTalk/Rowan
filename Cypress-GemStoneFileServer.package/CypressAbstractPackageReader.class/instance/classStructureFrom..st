"
notice: This work is protected by copyright. All rights reserved.
category: private
"
classStructureFrom: classPropertiesDict

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself
