"
notice: This work is protected by copyright. All rights reserved.
category: private
"
classStructureFrom: classPropertiesDict comment: classComment

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself
