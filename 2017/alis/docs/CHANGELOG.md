# ALIS Change Log

History of all of the changes made to ALIS

This is the ```CHANGELOG.md``` file for [ALIS](https://github.com/axvr/alis). Created by [Alex Vear - axvr](https://github.com/axvr).

This project is licenced under the [MIT Licence](https://github.com/axvr/alis/blob/master/LICENCE).

The format is based on [Keep a Changelog](http://keepachangelog.com/) and it also adheres to [Semantic Versioning](http://semver.org/).

<!-- Styled in this format:

## [Version Information] - YEAR-MONTH-DATE

### Added
* List all items added
* If  this section contains nothing
* don't bother to include it within
* the ``CHANGELOG.md`` file

### Changed
* List all items changed
* If  this section contains nothing
* don't bother to include it within
* the ``CHANGELOG.md`` file

### Fixed
* List all items fixed
* If  this section contains nothing
* don't bother to include it within
* the ``CHANGELOG.md`` file

### Removed
* List all items removed
* If  this section contains nothing
* don't bother to include it within
* the ``CHANGELOG.md`` file

### Translations
* List all sections translated here
* If this section contains nothing
* don't bother to include it within
* the ``CHANGELOG.md`` file

Leave 3 spaces between previous change log item -->

---

## [Unreleased -> 0.2.1] - 2017-07-27

### Fixed
* Removed dependency on "sudo" (to access all features: run as root).



## [Unreleased -> 0.2.0] - 2017-07-26

### Added
* Multi-language support

### Changed
* Full rewrite of ALIS to allow for many new features
* New help & usage menu
* Split sections into separate modules
* Reduce chance of log file corruption
* Simpler method of starting ALIS
* Whipatail module created; allows for quicker window creation, and dynamic window sizing
* Updated ```README.md``` to include new features

### Removed
* Most of the old code



## [Unreleased -> 0.1.1] - 2017-03-19

### Fixed
* Code for Whiptail not working
* Version labels and CHANGELOG updated



## [Unreleased -> 0.1.0] - 2017-03-19

### Added
* Improved partition selection screen
* More options in the Pre-Installation menu
* Badges now on the ```README.md``` file
* Partition secure wipe system
* Allow manual partition creation by the user
* ```CODE_OF_CONDUCT.md``` file added and linked to within other documents
* Added a hidden CHANGELOG style guide within the raw ``CHANGELOG.md`` file

### Changed
* Change current version information
* Modified Code of Conduct
* Other minor changes

### Fixed
* Partition script restructure and refactor of code
* Fixed all found typos
* Improved Contributing document
* All links within all of the documents
* The ``CHANGELOG.md`` formatting issue
* Broken ``CONTRIBUTING.md`` Licence link
* The ``conf_files/pre_installation/partition.pl`` select partitions whiptail command subroutine
* Fixed alignment issue with parameters in ``start.pl``
* Fixed errors in the about screen

### Removed
* The old code for the old select partitions
* Removed old code of conducts (yes there were two)



## [Unreleased -> 0.0.6] - 2017-02-10

### Added
* The beginnings of the partitioning script (unstable)
* New Licence - MIT

### Fixed
* Replaced all of the file headers with the new version; including the new Licence



## [Unreleased -> 0.0.5] - 2017-02-04

### Added
* Added the Licence information to all ALIS files
* Added system compatibility information to ``README.md``
* Added ``README.md`` section containing information about all ALIS arguments
* Added ``README.md`` section on the Arch Linux i686 support drop
* Added log file full implementation to ``main-menu.pl``
* Added templates for the other scripts

### Fixed
* Fixed grammatical and spelling errors in ``README.md``
* Fixed formatting issue in ``CHANGELOG.md``
* Finished ``CONTRIBUTING.md`` document
* Minor other ``README.md`` changes
* Finished most of ``main-menu.pl`` (no more can be done yet)
* Changed all ``exit`` instructions to ``exit X`` where X is the appropriate exit number
* Commented both ``start.pl`` and ``main-menu.pl``

### Removed
* Dropped 32 bit support



## [Unreleased -> 0.0.4] - 2017-01-23

### Fixed
* Edited ``README.md`` to include download and run instructions
* Edited ``main-menu.pl`` and added a few more menu options
