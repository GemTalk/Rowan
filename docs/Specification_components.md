## Components

### Components
A **component** is the loadable unit in Rowan.
A **component** specifies a complete and independent unit of functionality.
A **component** has a name and is located on disk in the *components directory*, specified by the *components path* of the *project specification*.

A **component** is composed of:
- A list of *dependent project names*. When a **component** is loaded, the *dependent projects* are loaded as well.
- A *preload doit name*. 
The *preload doit* is performed before the **component** itself is loaded.
- A *postload doit name*. 
The *postload doit* is performed after the **component** itself is loaded.
- A list of *package names*. 
When a **component** is loaded, the listed packages are loaded.
- A list of *component names*. 
When a **component** is loaded, the packages listed by the referenced components are loaded.
The list may reference **components** or **subcomponents** by name and recursive references (direct or indirect) are allowed.

#### Components and Tests
A **component** should have an associated set of *tests* that validates the public API for the component.
As a loadable unit there is an implied contract between the developers and users of the component that the public API implemented by the classes and methods loaded by the component and the test* are expected to validate that contract.

The tests may be defined by packages named in the component itself, or they may be defined by packages named in subcomponents of the component. It is recommended that test* be defined in one or more `Tests` subcomponents that are conditional on a `test` *conditional attribute*. In this way end users can control whether or not they want tests loaded into an image.
### Subcomponents
A **subcomponent** has a name and is located on disk in the *components directory* or a subdirectory of the *components directory*. If the **subcomponent** is located in a subdirectory, then the name of the **subcomponent** must be the path to the **subcomponent** relative to the *components directory*.

Similar to the **component**, a **subcomponent** is composed of:
- A list of *dependent project names*.
- A *preload doit name*. 
- A *postload doit name*. 
- A list of *package names*. 
- A list of *component names*.

#### Conditional Subcomponents
A **subcomponent** has a *condition* associated with it. 
A *condition* is a single *conditional attribute*.

If a **subcomponent** is referenced either directly or indirectly from a **component** that is slated to be loaded (i.e., listed in the *component names* of the *load specification* used to *resolve* the *project*), the **subcomponent** *condition* is matched against the *platform conditional attributes* and if there is a match, then the **subcomponent** will be `active` and it's packages, components, doits and projects will participate in the load.

A **subcomponent** that references another **subcomponent** via its list of *component names* is an effective `AND` of the two *conditions* for the **referenced subcomponent**

#### Subcomponents and Tests
A **subcomponent** may only be loaded via a direct or indirect reference from a **component**.
There is no implied API for a **subcomponent**, so no associated *test* are expected.

### PlatformComponents
A **platformComponent** is a special case of **subcomponent**, where the *condition* is a collection of *conditional attributes* and/or *platform version patterns*.
Each of the items in the list are effectively ORed together.
The use case for ORing is when you have a set of packages that are conditional on a set of platforms. For example here are several **platformComponent** *conditions*:
1. `#( 'gemstone' 'pharo' )`
2. `#( 'gs3.2.1.x' 'pharo7.x' )`
3. `#( 'gs3.[1-]' 'pharo7.[1-5]' )`

If you are familiar with [Metacello][1], the the *conditions* in items 1 and 2 should be familiar to you.

The `#( 'gemstone' 'pharo' )` *condition* in item 1 is a case where no pattern matching is needed, if the *platform conditional attributes list* contains wither 'gemstone' or 'pharo' the *condition* is matched and the **subcomponent** is active. 
For these *exact match* cases, it isn't necessary that the attributes in the pattern list be the name of a platform.
However, items 2 and 3, involves matching platform specific version strings.

The `#( 'gs3.2.1.x' 'pharo7.x' )` *condition* where the `x` in each pattern may be replaced with a continuation of the version string.
The pattern  'gs3.2.1.x' will match all of the following gemstone version strings: '3.2.1.0', '3.2.1.2', '3.2.1.2.5', '3.2.1.10' and not match the following gemstone version strings: '3.2.1', '3.2.2'.

From experience with Metacello, t is clear that a pattern is also needed that allows for matching version ranges and the example in item 3 illustrates the version range pattern.
The pattern 'gs3.[1-]' will do an open ended match ot gemstone version strings from '3.1.0' up to, but not including version '4.0'.
The pattern 'pharo7.[1-5]' will match pharo version strings in a closed range from '7.1' through '7.5'.

### Component directory structure
As mentioned earlier, it is expected that **components** be located in the *components directory*. It then follows that **subcomponents** should be located in subdirectories, to segregate the **subcomponents** and **components**.
Since **subcomponents** have a *condition* it makes sense to organize **subcomponents** by the value of their *conditions*

### CategoryComponent and CategorySubcomponent
The **categoryComponent** and **categorySubcomponent** are used to provide code organization within a package/class browser.

![Jadeite Project Browser](,/Jadeite_project_browser.png?raw=true "Jadeite Project Browser")

[1]: https://github.com/dalehenrich/metacello-work
