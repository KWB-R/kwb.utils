# Changes since last release

* add argument "deuOnly" to substSpecialChars()

# [kwb.utils 0.11.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.11.0) <small>2022-01-19</small>

* add argsCsv(), moved from kwb.sema

# [kwb.utils 0.10.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.10.0) <small>2022-01-19</small>

* add findChanges(), to be used by kwb.utils::hsEventsOnChange()
* add toConditional()
* allow to pass a "path" to getAttribute()

# [kwb.utils 0.9.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.9.0) <small>2021-11-16</small>

* fix bug in multiColumnLookup(): Consider "value" argument
* fix bug in hsStringToDate(): Do not complain on NA that were already in x
* Add new private functions: enumeration(), hintAvailable(), hintNoSuch(),
  stopIsNotBut(), 
* checkForMissingColumns(), selectElements(): Use new private functions

# [kwb.utils 0.8.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.8.0) <small>2021-02-19</small>

* add: matrixToDataFrame()
* add (private): get_cache_dir(), set_cache_dir()
* add: readLinesWithEncoding(), moved from fakin.path.app::read_lines()
* get_cached(): add argument "dbg"; return NULL if file does not exist
* lastElement(): do not use tail() for lists, use x[[length(x)]] instead
* orderBy(): allow to order by multiple columns
* preparePdf(): set paper to "special" if width or height are set
* readDictionary(): add arguments "fileEncoding", "...", pass on to
  readLinesWithEncoding()
* writeDictionary(): check "file" argument. Do not check the file path unless
  "file" is of class character (and not a connection)

# [kwb.utils 0.7.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.7.0) <small>2020-07-31</small>

* new functions: backspace(), createAccessor(), createStorage(), left(),
  listToDepth(), objectToText(), orderBy(), repeated(), reproducibleSample(),
  right(), space(), substSpecialChars(), textToObject()
* new generic (S3) function: print.repro_sample
* listToDepth(): Avoid endless recursion by checking for empty directory names,
  add argument "template"
* hsSubstSpecChars(): Add upper case umlauts, rename to substSpecialChars(),
  make deprecated
* .askForPassword(): Set default "NULL" for argument "account"; return invisible
* assignGlobally(): Make deprecated and do not use internally any more
* getPassword(): Return invisible
* hsPrepPdf(): Do not use internally any more
* warningDeprecated(): Add argument "parentheses"

# [kwb.utils 0.5.1](https://github.com/KWB-R/kwb.utils/releases/tag/v0.5.1) <small>2020-04-20</small>

* Fix bug in moveColumnsToFront(): keep data frame structure in case that the
  data frame has only one column
* createMatrix(): Set defaults for name.row and name.col to NULL (was: "")

# [kwb.utils 0.5.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.5.0) <small>2019-12-17</small>

* new: toPdf()
* new: assertFinalSlash()
* new private functions:
  + cache_and_return()
  + clear_cache()
  + get_cached()
  + get_cached_file()
  + run_cached()
  + objectToText()
  + textToObject()

# [kwb.utils 0.4.4](https://github.com/KWB-R/kwb.utils/releases/tag/v0.4.4) <small>2019-08-10</small>

* improved documentation (deploy docu also from "dev" branch, include NEWS.md)
* fixed continious integration (Windows)

# [kwb.utils 0.4.3](https://github.com/KWB-R/kwb.utils/releases/tag/v0.4.3) <small>2019-08-04</small>

* new: findPartialDuplicates()
* new: removeDuplicates()
* add arg "dbg" to excludeNULL()


# [kwb.utils 0.4.2](https://github.com/KWB-R/kwb.utils/tree/773c632aaa631ad51057eb99a841bb5f2721968c) <small>2019-03-26</small>

* export log-functions: .log(), .logline(), .logok(), .logstart()
* new: multiColumnLookup()

# [kwb.utils 0.4.1](https://github.com/KWB-R/kwb.utils/tree/37245e933ceed6538220caaadfc1aa4b2d7fc6ee) <small>2019-03-23</small>

* export functions explicitly, do not export all functions starting with dot "."
* new: isDotOrDoubleDot(), isASCII(), moved from kwb.fakin
* add arg "log_time" to catAndRun() (2019-01-18)
* remove non-existing links in documentation (2019-01-18)
* new: encode(), decode(), printable_chars() (2019-01-03)
* new: intToNumeralSystem() (2018-12-21)
* new: createIdAlong() (2018-12-16)


# [kwb.utils 0.4.0](https://github.com/KWB-R/kwb.utils/releases/tag/v0.4.0) <small>2018-11-19</small>

* new: combineAlternatingly()
* bug fix in .defaultIf(): was not working as expected with x = NULL
* package prepared for automatic tests
* new test for .defaultIf()


# [kwb.utils 0.2.1](https://github.com/KWB-R/kwb.utils/releases/tag/v.0.2.1) <small>2016-09-06</small>

First release on GitHub

* hsPrepPdf, hsShowPdf moved from kwb.base
* hsCatLists removed (it was just the same as c()!)

# kwb.utils 0.1.0 <small>unreleased</small>

* creation of the package
