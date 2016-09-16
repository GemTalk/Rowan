"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
