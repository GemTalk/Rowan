"
notice: This work is protected by copyright. All rights reserved.
category: private
"
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
