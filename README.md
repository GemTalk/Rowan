# CypressReferenceImplementation: a GemStone implementation of the Smalltalk file format for interchange and version control.

The current implementation (still in development) has been developed against GemStone/S 3.2. It almost certainly will have issues
against previous versions of GemStone/S. Dale Henrichs has ported it to GLASS.

There are a number of issues with the current version (see below).


### GemStone Server Installation

There is a Topaz installation script, install.tpz, which can be used to bootstrap the image. Note that the GemStone fileout always
ends methods with a LF character, and that the actual Cypress code may not include a final line terminator. Once the .gs files are 
loaded, use the Cypress Package Manager to reload the actual Cypress implementation.

The GemStone/S implementation is a pure server implementation and lacks a user interface. There is a client user interface available for
GBS under VisualWorks v7.9.1. It almost certainly will have issues under older versions of VisualWorks.

### GBS/VW Client Installation

There are two parcels which needed to be loaded into your VisualWorks development environment, extending GBS. GbxDifferencingTools.pcl
provides basic GemStone/S differences visualization. CypressDifferencingTools.pcl provides a simplistic user interface for interacting
with the Cypress Package Manager server class (CypressPackageManager). There is a second version of the package manager under development,
but it is incomplete (CypressPackageManager2). The unit tests are written against the second version, which has no user interface yet.

### Server Functionality Documentation

CypressPackageManager is the class to instantiate for manipulating Cypress packages. The GBS/VW client class CypressPackageManagerView
provides an example of how this class should be used. The premise is that package is found in a single repository only, which can be limiting.
This implementation make relatively simple use of replication and can potentially be extended for use in a non-GBS environment such as Pharo.

- #packageInformationList answers a list of CypressPackageInformation instances for possible and known packages in the image.
  (CypressPackageManager2 uses a hierachy of Package Information classes for richer behaviour.)
- #potentialPackageNames answers a list of possible package names found in the image, based on class categories and on method categories 
  which begin with an asterisk. This method is not normally used directly.
- #refreshPackageInformation is used to provide an updated list of Package Information instances, reflecting the latest image and file
  system representations of the packages.
- #lookForLoadedPackagesIn: updates the Package Information instances list with information for each package in the image which has
  a Cypress fileout in the directory specified by the argument.
- #lookForUnloadedPackagesIn: is similar, but adds Package Information instances for packages found in the specified directory, but
  not presently loaded.
- #updateSavedLocation:for: updates the Package Information with a new directory path in which the Cypress fileout will be saved or 
  will be found.
- #writePackageToDiskFrom: and #writePackagesToDiskFrom: are used to fileout the Cypress representation of a single package or a number
  of packages, respectively.
- #loadPackageFrom: loads the package specified in the argument (a Package Information instance).

CypressPackageManager2 is similar, but richer in scope. It is predicated on GBS replication, so will take more work
to implement in a non-GBS environment. In this model, a package can reside in a number of repositories, each of 
which can hold a different version from the others. Additionally, it supports URL-based repository identification. There are four supported repository 
formats (see the subclasses of CypressAbstractFileUrl). The URL scheme distinguishes the formats. ('cypress' is the pure Cypress format. 
'cypresslax' tolerates FileTree method formats, but writes Cypress format. 'cypressft' reads and writes using the FileTree method format.
'cypressfiletree' is a read-only scheme that reads FileTree format, but cannot overwrite it, thereby protecting the FileTree meta-data.)

CypressPackageManagerTest provides test cases for this upcoming Package Manager (note that some tests have no implementation yet).

### Issues
- The .gs fileouts used to bootstrap Cypress into the image add extra linefeeds at the end of methods.
  (This is unavoidable, due to the .gs file format, and will not be corrected.)
- The .gs fileouts have problems loading character values between 128 and 255 (e.g., accented characters).
  (By loading the actual Cypress version over it, the problem is eliminated during the bootstrap.)
- CypressPackageManager2 is incomplete, a work in progress.
- The only user interface offered requires GBS 7.6 and VisualWorks 7.9.1. (It may work with older versions, but hasn't been tested with any.)
- There is no GBS for VisualAge user interface provided yet.
- Older versions of GemStone/S will be problematic, as will older versions of VisualWorks.
- The code has been developed by and for SystemUser, with the bulk of the code loaded into SystemUser's UserGlobals Symbol Dictionary.
  (This means it is not yet accessible to other GemStone users.)
- There is no support for packages which load into a user's Globals and load extensions into Globals classes.
- CypressPackageManagerTest>>#testLoadingPackageBranchFromRepository is not yet written and the functionality is absent.
- CypressPackageManagerTest>>#testLoadingPackageWithGlobalExtensionWhenNotSystemUser is not yet written and the functionality is absent.
- CypressPackageManagerTest>>#testLoadingPackageFromGemStoneFileoutRepository is not yet written and the functionality is absent.
  It is a future possibility, not necessarily a planned feature.
- CypressPackageManagerTest>>#testLoadingPackageBranchFromRepository is not yet written and the functionality is absent.
  It is a future possibility, not necessarily a planned feature.
