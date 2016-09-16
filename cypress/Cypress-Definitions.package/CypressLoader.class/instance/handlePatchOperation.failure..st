"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
handlePatchOperation: aPatchOperation failure: anException
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.