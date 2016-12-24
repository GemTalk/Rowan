"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateFromImageCategory
	"My information about a class may be out of date. Update my information about the class.
	Things that might cause me to need updating:
	* The class may have been deleted or renamed.
	* The class may have been replaced by a new version.
 	* The class may have changed its category, which will move it to a different package.
	* The class comment may have changed."

	self handleClassDeletionOrNewVersion
		ifFalse: 
			[self
				updateForCategoryChange;
				updateForCommentChange]