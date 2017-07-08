"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classInstanceVariableNames

	^self properties at: 'classinstvars' ifAbsent: [#()]
