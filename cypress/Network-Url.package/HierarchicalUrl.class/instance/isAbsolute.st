"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: access
"
isAbsolute
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.