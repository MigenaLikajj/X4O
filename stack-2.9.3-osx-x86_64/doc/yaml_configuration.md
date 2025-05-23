<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Configuration and customisation

Stack is configured by the content of YAML files. Some Stack operations can also
be customised by the use of scripts.

## YAML configuration

Stack's YAML configuration options break down into
[project-specific](#project-specific-configuration) options and
[non-project-specific](#non-project-specific-configuration) options. They are
configured at the project-level or globally.

The **project-level** configuration file (`stack.yaml`) contains
project-specific options and may contain non-project-specific options.

Stack obtains project-level configuration from one of the following (in order of
preference):

1. A file specified by the `--stack-yaml` command line option.
2. A file specified by the `STACK_YAML` environment variable.
3. A file named `stack.yaml` in the current directory or an ancestor directory.
4. A file name `stack.yaml` in the `global-project` directory in the Stack root.

The **global** configuration file (`config.yaml`) contains only
non-project-specific options.

Stack obtains global configuration from a file named `config.yaml`. The location
of this file depends on the operating system and whether Stack is configured to
use the XDG Base Directory Specification.

=== "Unix-like"

    `config.yaml` is located in `/etc/stack` (for system-wide options); and/or
    in the Stack root (for user-specific options).

=== "Windows"

    `config.yaml` is located in the Stack root.

=== "XDG Base Directory Specification"

    On Unix-like operating systems and Windows, Stack can be configured to
    follow the XDG Base Directory Specification if the environment variable
    `STACK_XDG` is set to any non-empty value. However, Stack will ignore that
    configuration if the Stack root location has been set on the command line or
    the `STACK_ROOT` environment variable exists.

    If Stack is following the XDG Base Directory Specification, the location of
    `config.yaml` (for user-specific options) is `<XDG_CONFIG_HOME>/stack`. If
    the `XDG_CONFIG_HOME` environment variable does not exist, the default is
    `~/.config/stack` on Unix-like operating systems and `%APPDIR%\stack` on
    Windows.

This page is intended to document fully all YAML configuration options. If you
identify any inaccuracies or incompleteness, please update the page, and if
you're not sure how, open an issue labeled "question".

If you wish to understand the difference between a `stack.yaml` files and a
Cabal file (named `<package_name>.cabal`), see the
[stack.yaml vs a Cabal file](stack_yaml_vs_cabal_package_file.md) documentation.

## Project-specific configuration

Project-specific configuration options are valid only in a project-level
configuration file (`stack.yaml`).

> Note: We define **project** to mean a directory that contains a `stack.yaml`
> file, which specifies how to build a set of packages. We define **package** to
> be a package with a Cabal file or an Hpack `package.yaml` file.

In your project-specific options, you specify both **which local packages** to
build and **which dependencies to use** when building these packages. Unlike the
user's local packages, these dependencies aren't built by default. They only get
built when needed.

Shadowing semantics, described
[here](https://docs.haskellstack.org/en/v1.5.1/architecture/#shadowing), are
applied to your configuration. So, if you add a package to your `packages` list,
it will be used even if you're using a snapshot that specifies a particular
version. Similarly, `extra-deps` will shadow the version specified in the
resolver.

### resolver or snapshot

Command line equivalent (takes precedence): `--resolver` option

`resolver` and `snapshot` are synonyms. Only one of these keys is permitted, not
both.

The `resolver` or `snapshot` key specifies which snapshot is to be used for this
project. A snapshot defines a GHC version, a number of packages available for
installation, and various settings like build flags. It is called a resolver
since a snapshot states how dependencies are resolved. There are currently
four resolver types:

* LTS Haskell snapshots, e.g. `resolver: lts-19.17`
* Stackage Nightly snapshots, e.g. `resolver: nightly-2002-08-04`
* No snapshot, just use packages shipped with the compiler. For GHC this looks
  like `resolver: ghc-9.2.4`
* Custom snapshot, via a URL or relative file path. For further information, see
  the [Pantry](pantry.md) documentation.

Each of these resolvers will also determine what constraints are placed on the
compiler version. See the [compiler-check](#compiler-check) option for some
additional control over compiler version.

The `resolver` key corresponds to a Pantry snapshot location. For further
information, see the [Pantry](pantry.md) documentation.

### packages

Default:

~~~yaml
packages:
- .
~~~

_NOTE_ From Stack 1.11, Stack moved over to Pantry for managing extra-deps, and
removed some legacy syntax for specifying dependencies in `packages`. Some
conversion notes are provided below.

The `packages` key specifies a list of packages that are part of your local
project. These are specified via paths to local directories. The paths are
considered relative to the directory containing the `stack.yaml` file. For
example, if your `stack.yaml` is located at `/foo/bar/stack.yaml`, and you have:

~~~yaml
packages:
- hello
- there/world
~~~

Your configuration means "I have packages in `/foo/bar/hello` and
`/foo/bar/there/world`.

If these packages should be treated as dependencies instead, specify them in
`extra-deps` key, described below.

The `packages` key is _optional_. The default item, '`.`', means that your
project has exactly one package, and it is located in the current directory.

Each package directory specified must have a valid Cabal file or Hpack
`package.yaml` file present. The subdirectories of the directory are not
searched for Cabal files. Subdirectories will have to be specified as
independent items in the list of packages.

Project packages are different from snapshot dependencies (via `resolver`) and
extra dependencies (via `extra-deps`) in multiple ways, e.g.:

* Project packages will be built by default with a `stack build` without
  specific targets. Dependencies will only be built if they are depended upon.
* Test suites and benchmarks may be run for project packages. They are never run
  for extra dependencies.

__Legacy syntax__ Prior to Stack 1.11, it was possible to specify dependencies
in your `packages` configuration value as well. This support was removed to
simplify the file format. Instead, these values should be moved to `extra-deps`.
As a concrete example, you would convert:

~~~yaml
packages:
- .
- location:
    git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  extra-dep: true
- location:
    git: https://github.com/yesodweb/wai.git
    commit: 6bf765e000c6fd14e09ebdea6c4c5b1510ff5376
    subdirs:
      - wai-extra
  extra-dep: true

extra-deps:
  - streaming-commons-0.2.0.0
  - time-1.9.1
  - yesod-colonnade-1.3.0.1
  - yesod-elements-1.1
~~~

into

~~~yaml
packages:
- .

extra-deps:
  - streaming-commons-0.2.0.0
  - time-1.9.1
  - yesod-colonnade-1.3.0.1
  - yesod-elements-1.1
  - git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  - git: https://github.com/yesodweb/wai.git
    commit: 6bf765e000c6fd14e09ebdea6c4c5b1510ff5376
    subdirs:
      - wai-extra
~~~

And, in fact, the `packages` value could be left off entirely since it's using
the default value.

### extra-deps

Default: `[]`

This key allows you to specify extra dependencies on top of what is defined in
your snapshot (specified by the `resolver` key mentioned above). These
dependencies may either come from a local file path or a Pantry package
location.

For the local file path case, the same relative path rules as apply to
`packages` apply.

Pantry package locations allow you to include dependencies from three different
kinds of sources:

* Hackage
* Archives (tarballs or zip files, either local or over HTTP or HTTPS)
* Git or Mercurial repositories

Here's an example using all of the above:

~~~yaml
extra-deps:
- vendor/hashable
- streaming-commons-0.2.0.0
- time-1.9.1
- yesod-colonnade-1.3.0.1
- yesod-elements-1.1
- git: https://github.com/bitemyapp/esqueleto.git
  commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
- url: https://github.com/yesodweb/wai/archive/6bf765e000c6fd14e09ebdea6c4c5b1510ff5376.tar.gz
  subdirs:
    - wai-extra
- github: snoyberg/conduit
  commit: 2e3e41de93821bcfe8ec6210aeca21be3f2087bf
  subdirs:
    - network-conduit-tls
~~~

For further information on the format for specifying dependencies, see the
[Pantry](pantry.md) documentation.

### flags

Default: `{}`

Command line equivalent (takes precedence): `stack build --flag` option

Flags can be set for each package separately. For example:

~~~yaml
flags:
  package-name:
    flag-name: true
~~~

If a specified flag is different than the one specified for a snapshot package,
then the snapshot package will automatically be promoted to be an extra-dep.

### drop-packages

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Default: `[]`

Packages which, when present in the snapshot specified in `resolver`, should not
be included in our package. This can be used for a few different purposes, e.g.:

* Ensure that packages you don't want used in your project cannot be used in a
  `package.yaml` file (e.g., for license reasons)
* Prevent overriding of a global package like `Cabal`. For more information, see
  Stackage issue
  [#4425](https://github.com/commercialhaskell/stackage/issues/4425)
* When using a custom GHC build, avoid incompatible packages (see this
  [comment](https://github.com/commercialhaskell/stack/pull/4655#issuecomment-477954429)).

~~~yaml
drop-packages:
- Cabal
- buggy-package
- package-with-unacceptable-license
~~~

### user-message

If present, specifies a message to be displayed every time the configuration is
loaded by Stack. It can serve as a reminder for the user to review the
configuration and make any changes if needed. The user can delete this message
if the generated configuration is acceptable.

For example, a user-message is inserted by `stack init` when it omits packages
or adds external dependencies, namely:

~~~yaml
user-message: ! 'Warning: Some packages were found to be incompatible with the resolver
  and have been left commented out in the packages section.

  Warning: Specified resolver could not satisfy all dependencies. Some external packages
  have been added as dependencies.

  You can omit this message by removing it from stack.yaml

'
~~~

### custom-preprocessor-extensions

Default: `[]`

Command line equivalent: `--customer-preprocessor-extensions` option

In order for Stack to be aware of any custom preprocessors you are using, add
their extensions here

~~~yaml
custom-preprocessor-extensions:
- erb
~~~

TODO: Add a simple example of how to use custom preprocessors.

## Non-project-specific configuration

Non-project configuration options are valid in a project-level configuration
file (`stack.yaml`) or in global configuration files (`config.yaml`). The
options below are listed in alphabetic order.

### allow-different-user

[:octicons-tag-24: 1.0.1.0](https://github.com/commercialhaskell/stack/releases/tag/v1.0.1.0)

Restrictions: POSIX systems only.

Default: `false`

Command line equivalent (takes precedence): `--[no-]allow-different-user` flag

Allow users other than the owner of the Stack root to use the Stack
installation.

~~~yaml
allow-different-user: true
~~~

The intention of this option is to prevent file permission problems, for example
as the result of a Stack command executed under `sudo`.

The option is automatically enabled when Stack is re-spawned in a Docker
process.

### allow-newer

[:octicons-tag-24: 0.1.8.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.8.0)

Default: `false`

Whether to ignore version bounds in Cabal files. This also ignores lower bounds.
The name `allow-newer` is chosen to match the commonly-used Cabal option.


~~~yaml
allow-newer: true
~~~

### allow-newer-deps (experimental)

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default: `none`

Determines a subset of packages to which `allow-newer` should apply. This option
has no effect (but warns) if `allow-newer` is `false`.

~~~yaml
allow-newer-deps:
  - foo
  - bar
~~~

### apply-ghc-options

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Default: `locals`

Which packages do ghc-options on the command line get applied to? Before Stack
0.1.6, the default value was `targets`

~~~yaml
apply-ghc-options: locals # all local packages
# apply-ghc-options: targets # all local packages that are targets
# apply-ghc-options: everything # applied even to snapshot and extra-deps
~~~

Note that `everything` is a slightly dangerous value, as it can break invariants
about your snapshot database.

### arch

Default: The machine architecture on which Stack is running.

Command line equivalent (takes precedence): `--arch` option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`arch` sets the machine architecture. Values are those recognized by Cabal,
including `x86_64`, `i386` and `aarch64`.

### build

[:octicons-tag-24: 1.1.0](https://github.com/commercialhaskell/stack/releases/tag/v1.1.0)

Default:

~~~yaml
build:
  library-profiling: false
  executable-profiling: false
  copy-bins: false
  prefetch: false
  keep-going: false
  keep-tmp-files: false

  # NOTE: global usage of haddock can cause build failures when documentation is
  # incorrectly formatted.  This could also affect scripts which use Stack.
  haddock: false
  haddock-arguments:
    haddock-args: []      # Additional arguments passed to haddock, --haddock-arguments
    # haddock-args:
    # - "--css=/home/user/my-css"
  open-haddocks: false    # --open
  haddock-deps: false     # if unspecified, defaults to true if haddock is set
  haddock-internal: false

  # These are inadvisable to use in your global configuration, as they make the
  # Stack build command line behave quite differently.
  test: false
  test-arguments:
    rerun-tests: true   # Rerun successful tests
    additional-args: [] # --test-arguments
    # additional-args:
    # - "--fail-fast"
    coverage: false
    no-run-tests: false
  bench: false
  benchmark-opts:
    benchmark-arguments: ""
    # benchmark-arguments: "--csv bench.csv"
    no-run-benchmarks: false
  force-dirty: false
  reconfigure: false
  cabal-verbose: false
  split-objs: false

  # Since 1.8. Starting with 2.0, the default is true
  interleaved-output: true

  # Since 1.10
  ddump-dir: ""
~~~

Command line equivalents (take precedence): Yes, see below.

Allows setting build options which are usually specified on the command line.

The meanings of these settings correspond directly with the command line flags
of the same name. For further information, see the
[`stack build` command](build_command.md) documentation and the
[users guide](GUIDE.md#the-build-command).

### color

Command line equivalent (takes precedence): `--color` option

This option specifies when to use color in output. The option is used as
`color: <WHEN>`, where `<WHEN>` is 'always', 'never', or 'auto'. On Windows
versions before Windows 10, for terminals that do not support color codes, the
default is 'never'; color may work on terminals that support color codes.

(The British English spelling (colour) is also accepted. In yaml configuration
files, the American spelling is the alternative that has priority.)

### compiler

[:octicons-tag-24: 0.1.8.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.8.0)

Command line equivalent (takes precedence): `--compiler` option

Overrides the compiler version in the resolver. Note that the `compiler-check`
flag also applies to the version numbers. This uses the same syntax as compiler
resolvers like `ghc-9.2.4`. This can be used to override the
compiler for a Stackage snapshot, like this:

~~~yaml
resolver: lts-14.20
compiler: ghc-8.6.4
compiler-check: match-exact
~~~

#### Building GHC from source (experimental)

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Stack supports building the GHC compiler from source. The version to build and
to use is defined by a a Git commit ID and an Hadrian "flavour" (Hadrian is the
build system of GHC) with the following syntax:

~~~yaml
compiler: ghc-git-COMMIT-FLAVOUR
~~~

In the following example the commit ID is "5be7ad..." and the flavour is
"quick":

~~~yaml
compiler: ghc-git-5be7ad7861c8d39f60b7101fd8d8e816ff50353a-quick
~~~

By default the code is retrieved from the main GHC repository. If you want to
select another repository, set the "compiler-repository" option:

~~~yaml
compiler-repository: git://my/ghc/repository
# default
# compiler-repository: https://gitlab.haskell.org/ghc/ghc.git
~~~

Note that Stack doesn't check the compiler version when it uses a compiler built
from source. Moreover it is assumed that the built compiler is recent enough as
Stack doesn't enable any known workaround to make older compilers work.

Building the compiler can take a very long time (more than one hour). Hint: for
faster build times, use Hadrian flavours that disable documentation generation.

#### Global packages

The GHC compiler you build from sources may depend on unreleased versions of
some global packages (e.g. Cabal). It may be an issue if a package you try to
build with this compiler depends on such global packages because Stack may not
be able to find versions of those packages (on Hackage, etc.) that are
compatible with the compiler.

The easiest way to deal with this issue is to drop the offending packages as
follows. Instead of using the packages specified in the resolver, the global
packages bundled with GHC will be used.

~~~yaml
drop-packages:
- Cabal
- ...
~~~

Another way to deal with this issue is to add the relevant packages as
`extra-deps` built from source. To avoid mismatching versions, you can use
exactly the same commit id you used to build GHC as follows:

~~~
extra-deps:
- git: https://gitlab.haskell.org/ghc/ghc.git
  commit: 5be7ad7861c8d39f60b7101fd8d8e816ff50353a
  subdirs:
    - libraries/Cabal/Cabal
    - libraries/...
~~~

#### Bootstrapping compiler

Building GHC from source requires a working GHC (known as the bootstrap
compiler). As we use a Stack based version of Hadrian (`hadrian/build-stack` in
GHC sources), the bootstrap compiler is configured into `hadrian/stack.yaml` and
fully managed by Stack.

### compiler-check

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Default: `match-minor`

Specifies how the compiler version in the resolver is matched against concrete
versions. Valid values:

* `match-minor`: make sure that the first three components match, but allow
  patch-level differences. For example< 7.8.4.1 and 7.8.4.2 would both match
  7.8.4. This is useful to allow for custom patch levels of a compiler.
* `match-exact`: the entire version number must match precisely
* `newer-minor`: the third component can be increased, e.g. if your resolver is
  `ghc-7.10.1`, then 7.10.2 will also be allowed. This was the default up
  through Stack 0.1.3

### concurrent-tests

[:octicons-tag-24: 0.1.2.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.2.0)

Default: `true`

This option specifies whether test suites should be executed concurrently with
each other. The default is `true` since this is usually fine and it often means
that tests can complete earlier. However, if some test suites require exclusive
access to some resource, or require a great deal of CPU or memory resources,
then it makes sense to set this to `false`.

~~~yaml
concurrent-tests: false
~~~

### configure-options

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Options which are passed to the configure step of the Cabal build process.
These can either be set by package name, or using the `$everything`,
`$targets`, and `$locals` special keys. These special keys have the same
meaning as in `ghc-options`.

~~~yaml
configure-options:
  $everything:
  - --with-gcc
  - /some/path
  my-package:
  - --another-flag
~~~

### connection-count

Default: `8`

Integer indicating how many simultaneous downloads are allowed to happen.

### default-template

Default: `new-template` in the
[stack-templates](https://github.com/commercialhaskell/stack-templates/)
repository.

This option specifies which template to use with `stack new`, when none is
specified. Other templates are listed in the
[stack-templates](https://github.com/commercialhaskell/stack-templates/)
repository. See the output of `stack templates`.

### docker

Command line equivalents: `--docker-*` flags and options (see
`stack --docker-help` for details).

For further information, see the
[Docker integration](docker_integration.md#configuration) documentation.

### dump-logs

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

Default: `warning`

Command line equivalent (takes precedence): `--[no-]dump-logs` flag

In the case of *non-interleaved* output and *more than one* target package,
Stack sends the build output from GHC for each target package to a log file,
unless an error occurs. For further information, see the
[`stack build --[no-]interleaved-output` flag](build_command.md#the-stack-build---no-interleaved-output-flag)
documentation.

The value of the `dump-logs` key controls what, if any, log file content is sent
('dumped') to the console at the end of the build. Possible values are:

~~~yaml
dump-logs: none      # don't dump the content of any log files
dump-logs: warning   # dump the content of log files that are warnings
dump-logs: all       # dump all of the content of log files
~~~

At the command line, `--no-dump-logs` is equivalent to `dump-logs: none` and
`--dump-logs` is equivalent to `dump-logs: all`.

### extra-include-dirs

Default: `[]`

Command line equivalent: `--extra-include-dirs` option (repeat for each
directory)

A list of extra paths to be searched for header files. Paths should be absolute

~~~yaml
extra-include-dirs:
- /opt/foo/include
~~~

Since these are system-dependent absolute paths, it is recommended that you
specify these in your `config.yaml` file. If you control the build environment
in your project's ``stack.yaml``, perhaps through docker or other means, then it
may well make sense to include these there as well.

### extra-lib-dirs

Default: `[]`

Command line equivalent: `--extra-lib-dirs` option (repeat for each directory)

A list of extra paths to be searched for libraries. Paths should be absolute

~~~yaml
extra-lib-dirs:
- /opt/foo/lib
~~~

Since these are system-dependent absolute paths, it is recommended that you
specify these in your `config.yaml` file. If you control the build environment
in your project's ``stack.yaml``, perhaps through Docker or other means, then it
may well make sense to include these there as well.

### extra-path

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

This option specifies additional directories to prepend to the PATH. These will
be used when resolving the location of executables, and will also be visible in
the PATH of processes run by Stack.

For example, to prepend `/path-to-some-dep/bin` to your PATH:

~~~yaml
extra-path:
- /path-to-some-dep/bin
~~~

Other paths added by Stack - things like the project's binary directory and the
compiler's binary directory - will take precedence over those specified here
(the automatic paths get prepended).

### ghc-build

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

Default: `standard`

Command line equivalent (takes precedence): `--ghc-build` option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`ghc-build` specifies a specialized architecture for the GHC executable.
Normally this is determined automatically, but it can be overridden. Possible
arguments include `standard`, `gmp4`, `nopie`, `tinfo6`, `tinfo6-nopie`,
`ncurses6`, `int-native` and `integersimple`.

### ghc-options

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Allows specifying per-package and global GHC options:

~~~yaml
ghc-options:
    # All packages
    "$locals": -Wall
    "$targets": -Werror
    "$everything": -O2
    some-package: -DSOME_CPP_FLAG
~~~

Since Stack 1.6.0, setting a GHC options for a specific package will
automatically promote it to a local package (much like setting a custom package
flag). However, setting options via `$everything` on all flags will not do so
(see
[GitHub discussion](https://github.com/commercialhaskell/stack/issues/849#issuecomment-320892095)
for reasoning). This can lead to unpredictable behavior by affecting your
snapshot packages.

The behavior of the `$locals`, `$targets`, and `$everything` special keys
mirrors the behavior for the
[`apply-ghc-options` setting](#apply-ghc-options), which affects command line
parameters.

!!! note

    Prior to Stack 1.6.0, the `$locals`, `$targets`, and `$everything` keys
    were not supported. Instead, you could use `"*"` for the behavior
    represented now by `$everything`. It is highly recommended to switch to the
    new, more expressive, keys.

### ghc-variant

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

Default: `standard`

Command line equivalent (takes precedence): `--ghc-variant` option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`ghc-variant` specifies a variant of the GHC executable. Known values are:

* `standard`: Use the standard GHC binary distribution
* `int-native`: From GHC 9.4.1, use a GHC bindist that uses the Haskell-native
   big-integer
  [backend](https://downloads.haskell.org/~ghc/9.0.2/docs/html/users_guide/9.0.1-notes.html#highlights).
  For further information, see this [article](https://iohk.io/en/blog/posts/2020/07/28/improving-haskells-big-numbers-support/).
* `integersimple`: Use a GHC bindist that uses
  [integer-simple instead of GMP](https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes)
* any other value: Use a custom GHC bindist. You should specify
  [setup-info](#setup-info) or [setup-info-locations](#setup-info-locations)
  so `stack setup` knows where to download it,
  or pass the `stack setup --ghc-bindist` argument on the command-line

This option is incompatible with `system-ghc: true`.

### hackage-base-url

[:octicons-tag-24: 1.9.1](https://github.com/commercialhaskell/stack/releases/tag/v1.9.1)

Default: `https://hackage.haskell.org/`

Sets the address of the Hackage server to upload the package to.

~~~yaml
hackage-base-url: https://hackage.example.com/
~~~

### hide-source-paths

Default: `true`
([:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1))

Whether to use the `-fhide-source-paths` option by default for GHC >= 8.2:

~~~yaml
hide-source-paths: false
~~~

Build output when enabled:

~~~text
...
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_test_pr
...
~~~

Build output when disabled:

~~~text
...
[1 of 2] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Lib.o )
...
~~~

### hide-th-loading

Default: `true`

Strip out the "Loading ..." lines from GHC build output, produced when using
Template Haskell.

### ignore-revision-mismatch

(Removed 1.11)

This flag was introduced in Stack 1.6, and removed on the move to Pantry. You
will receive a warning if this configuration value is set.

### install-ghc

Default: `true`
([:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0))

Command line equivalent (takes precedence): `--[no-]install-ghc` flag

Whether or not to automatically install GHC when necessary.

### jobs

Default: the number of processors reported by your CPU.

Command line equivalent (takes precedence): `-j`, `--jobs` option

Specifies how many build tasks should be run in parallel. One usage for this
might be to avoid running out of memory by setting it to 1, like this:

~~~yaml
jobs: 1
~~~

### local-bin-path

Default (on Unix-like operating systems): `~/.local/bin`

Default (on Windows): `%APPDATA%\local\bin`

Command line equivalent (takes precedence): `--local-bin-path` option

Target directory for `stack install` and `stack build --copy-bins`.

### local-programs-path

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

The behaviour of this option differs between Unix-like operating systems and
Windows.

=== "Unix-like"

    Default: `programs` directory in the Stack root.

    This overrides the location of the Stack 'programs' directory, where tools
    like GHC get installed.

=== "Windows"

    Default: `%LOCALAPPDATA%\Programs\stack`, if the `%LOCALAPPDATA%`
    environment variable exists.

    This overrides the location of the Stack 'programs' directory, where tools
    like GHC and MSYS2 get installed.

    !!! warning

        If there is a space character in the `%LOCALAPPDATA%` path (which may be
        the case if the relevant user account name and its corresponding user
        profile path have a space) this may cause problems with building
        packages that make use of the GNU project's `autoconf` package and
        `configure` shell script files. That may be the case particularly if
        there is no corresponding short name ('8 dot 3' name) for the directory
        in the path with the space (which may be the case if '8 dot 3' names
        have been stripped or their creation not enabled by default). If there
        are problems building, it will be necessary to override the default
        location of Stack's 'programs' directory to specify an alternative path
        that does not contain space characters. Examples of packages on
        Hackage that make use of `configure` are `network` and `process`.

### modify-code-page

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Restrictions: Windows systems only.

Default: `true`

Command line equivalent (takes precedence): `--[no-]modify-code-page` flag

Whether to modify the code page for UTF-8 output.

~~~yaml
modify-code-page: false
~~~

### nix

[:octicons-tag-24: 0.1.10.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.10.0)

Default:

~~~yaml
nix:
  add-gc-roots: false
  enable: false
  nix-shell-options: []
  packages: []
  path: []
  pure: true
  shell-file:
~~~

Command line equivalents: `--nix-*` flags and options (see `stack --nix-help`
for details).

For further information, see the
[Nix integration](nix_integration.md#configuration) documentation.

### package-index

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default:

~~~yaml
package-index:
  download-prefix: https://hackage.haskell.org/
  hackage-security:
    keyids:
    - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
    - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
    - 2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3
    - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
    - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3
    ignore-expiry: true
~~~

Takes precedence over the `package-indices` key, which is deprecated.

Specify the package index. The index must use the
[Hackage Security](https://hackage.haskell.org/package/hackage-security) format.
This setting is most useful for providing a mirror of the official Hackage
server for

* bypassing a firewall; or
* faster downloads.

If the setting specifies an index that does not mirror Hackage, it is likely
that will result in significant breakage, including most snapshots failing to
work.

In the case of Hackage, the keys of its root key holders are contained in the
`haskell-infra/hackage-root-keys`
[repository](https://github.com/haskell-infra/hackage-root-keys). The Hackage
package index is signed. A signature is valid when three key holders have
signed. The Hackage timestamp is also signed. A signature is valid when one key
holder has signed.

If the `hackage-security` key is absent, the Hackage Security configuration will
default to that for the official Hackage server.

`key-threshold` specifies the minimum number of keyholders that must have signed
the package index for it to be considered valid.

`ignore-expiry` specifies whether or not the expiration of timestamps should be
ignored.

### package-indices

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Deprecated in favour of [`package-index`](#package-index), which takes
precedence if present.

Default:

~~~yaml
package-indices:
- download-prefix: https://hackage.haskell.org/
  hackage-security:
    keyids:
    - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
    - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
    - 2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3
    - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
    - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3
    ignore-expiry: true
~~~

!!! note

    Before Stack 2.1.3, the default for `ignore-expiry` was `false`. For more
    information, see
    [issue #4928](https://github.com/commercialhaskell/stack/issues/4928).

!!! note

    Before Stack 2.1.1, Stack had a different approach to `package-indices`. For
    more information, see
    [issue #4137](https://github.com/commercialhaskell/stack/issues/4137).

Specify the package index. For further information, see the `package-index`
[documentation](#package-index).

### pvp-bounds

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

Default: `none`

Command line equivalent (takes precedence): `stack sdist --pvp-bounds` option or
`stack upload --pvp-bounds` option

!!! warning

    As of Stack 1.6.0, this feature does not reliably work, due to issues with
    the Cabal library's printer. Stack will generate a warning when a lossy
    conversion occurs, in which case you may need to disable this setting. For
    further information, see issue
    [#3550](https://github.com/commercialhaskell/stack/issues/3550).

When using the `sdist` and `upload` commands, this setting determines whether
the Cabal file's dependencies should be modified to reflect PVP lower and upper
bounds.

#### Basic use

Values are `none` (unchanged), `upper` (add upper bounds), `lower` (add
lower bounds), and both (and upper and lower bounds). The algorithm it follows
is:

* If an upper or lower bound already exists on a dependency, it's left alone
* When adding a lower bound, we look at the current version specified by
  `stack.yaml`, and set it as the lower bound (e.g., `foo >= 1.2.3`)
* When adding an upper bound, we require less than the next major version
  (e.g., `foo < 1.3`)

~~~yaml
pvp-bounds: none
~~~

For further information, see the announcement
[blog post](https://www.fpcomplete.com/blog/2015/09/stack-pvp).

#### Use with Cabal file revisions

[:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0)

Each of the values listed above supports adding `-revision` to the end of the
value, e.g. `pvp-bounds: both-revision`. This means that, when uploading to
Hackage, Stack will first upload your tarball with an unmodified Cabal file, and
then upload a Cabal file revision with the PVP bounds added.

This can be useful - especially combined with the
[Stackage no-revisions feature](http://www.snoyman.com/blog/2017/04/stackages-no-revisions-field) -
as a method to ensure PVP compliance without having to proactively fix bounds
issues for Stackage maintenance.

### recommend-stack-upgrade

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

When Stack notices that a new version of Stack is available, should it notify
the user?

~~~yaml
recommend-stack-upgrade: true
~~~

### rebuild-ghc-options

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Default: `false`

Should we rebuild a package when its GHC options change? Before Stack 0.1.6,
this was a non-configurable `true`. However, in most cases, the flag is used to
affect optimization levels and warning behavior, for which GHC itself doesn't
actually recompile the modules anyway. Therefore, the new behavior is to not
recompile on an options change, but this behavior can be changed back with the
following:

~~~yaml
rebuild-ghc-options: true
~~~

### require-stack-version

Default: `"-any"`

Require a version of Stack within the specified range
([cabal-style](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information))
to be used for this project. Example: `require-stack-version: "== 0.1.*"`

### save-hackage-creds

[:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0)

Default: `true`

Controls whether, when using `stack upload`, the user's Hackage username and
password are stored in a local file.

~~~yaml
save-hackage-creds: true
~~~

### setup-info

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

The `setup-info` dictionary specifies download locations for tools to be
installed during set-up, such as GHC or, on Windows, 7z and MSYS2. The
dictionary maps `('Tool', 'Platform', 'Version')` to the location where it can
be obtained. For example, mapping `(GHC, 64-bit Windows, 9.2.3)` to the URL
hosting the archive file for GHC's installation.

Possible usages of this configuration option are:

1. Using Stack offline or behind a firewall.
2. Extending the tools known to Stack, such as cutting-edge versions of GHC or
   builds for custom Linux distributions (for use with the
   [ghc-variant](#ghc-variant) option).

By default, Stack obtains the dictionary from
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml).

The `setup-info` dictionary is constructed in the following order:

1. `setup-info` in the YAML configuration - inline configuration
2. `--setup-info-yaml` command line arguments - URLs or paths. Multiple
   locations may be specified.
3. `setup-info-locations` in the YAML configuration - URLs or paths. See further
   below.

The format of this key is the same as in the default
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml).
For example, GHC 9.2.3 of custom variant `myvariant` (see further below) on
64-bit Windows:

~~~yaml
setup-info:
  ghc:
    windows64-custom-myvariant:
      9.2.3:
        url: "https://example.com/ghc-9.2.3-x86_64-unknown-mingw32-myvariant.tar.xz"
~~~

'Platforms' are pairs of an operating system and a machine architecture (for
example, 32-bit i386 or 64-bit x86-64) (represented by the
`Cabal.Distribution.Systems.Platform` type). Stack currently (version 2.9.1)
supports the following pairs in the format of the `setup-info` key:

|Operating system|I386 arch|X86_64 arch|Other machine architectures                                 |
|----------------|---------|-----------|------------------------------------------------------------|
|Linux           |linux32  |linux64    |AArch64: linux-aarch64, Arm: linux-armv7, Sparc: linux-sparc|
|OSX             |macosx   |macosx     |                                                            |
|Windows         |windows32|windows64  |                                                            |
|FreeBSD         |freebsd32|freebsd64  |AArch64: freebsd-aarch64                                    |
|OpenBSD         |openbsd32|openbsd64  |                                                            |

For GHC, the distinguishing 'Version' in the key format includes a 'tag' for
any (optional) GHC variant (see [ghc-variant](#ghc-variant)) and a further 'tag'
for any (optional) specialised GHC build (see [ghc-build](#ghc-build)).

The optional variant 'tag' is either `-integersimple` or
`-custom-<custom_variant_name>`.

For example, for GHC 9.0.2 of specialised GHC build `tinfo6` on x86_64 Linux:
~~~yaml
setup-info:
  ghc:
    linux64-tinfo6:
      9.0.2:
        url: "http://downloads.haskell.org/~ghc/9.0.2/ghc-9.0.2a-x86_64-fedora27-linux.tar.xz"
        content-length: 237286244
        sha1: affc2aaa3e6a1c446698a884f56a0a13e57f00b4
        sha256: b2670e9f278e10355b0475c2cc3b8842490f1bca3c70c306f104aa60caff37b0
~~~

On Windows, the required 7z executable and DLL tools are represented in the
format of the `setup-info` key simply by `sevenzexe-info` and `sevenzdll-info`.

This configuration **adds** the specified setup information metadata to the
default. Specifying this configuration **does not** prevent the default
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml)
from being consulted as a fallback. If, however, you need to **replace** the
default `setup-info` dictionary, use the following:

~~~yaml
setup-info-locations: []
~~~

### setup-info-locations

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

Command line equivalent (takes precedence): `--setup-info-yaml` option

By way of introduction, see the [`setup-info`](#setup-info) option. This option
specifies the location(s) of `setup-info` dictionaries.

The first location which provides a dictionary that specifies the location of a
tool - `('Tool', 'Platform', 'Version')` - takes precedence. For example, you
can extend the default tools, with a fallback to the default `setup-info`
location, as follows:

~~~yaml
setup-info-locations:
- C:/stack-offline/my-stack-setup.yaml
- relative/inside/my/project/setup-info.yaml
- \\smbShare\stack\my-stack-setup.yaml
- http://stack-mirror.com/stack-setup.yaml
# Fallback to the default location
- https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml
~~~

Stack only refers to the default `setup-info` location if no locations are
specified in the `setup-info-locations` configuration or on the command line
using the `--setup-info-yaml` option.

For example, both of the following will cause `stack setup` not to consult the
default `setup-info` location:

~~~yaml
setup-info-locations:
- C:/stack-offline/my-stack-setup.yaml
~~~

and

~~~yaml
setup-info-locations: []
~~~

Relative paths are resolved relative to the `stack.yaml` file (either the one in
the local project or the global `stack.yaml`).

Relative paths may also be used for the installation paths to tools (such as GHC
or 7z). This allows vendoring the tools inside a monorepo (a single repository
storing many projects). For example:

Directory structure:

~~~text
- src/
- installs/
  - my-stack-setup.yaml
  - 7z.exe
  - 7z.dll
  - ghc-9.2.3.tar.xz
- stack.yaml
~~~

In the project's `stack.yaml`:

~~~yaml
setup-info-locations:
- installs/my-stack-setup.yaml
~~~

In `installs/my-stack-setup.yaml`:

~~~yaml
sevenzexe-info:
  url: "installs/7z.exe"

sevenzdll-info:
  url: "installs/7z.dll"

ghc:
  windows64:
    9.2.3:
      url: "installs/ghc-9.2.3.tar.xz"
~~~

### skip-ghc-check

Default: `false`

Command line equivalent (takes precedence): `--[no-]skip-ghc-check` flag

Should we skip the check to confirm that your system GHC version (on the PATH)
matches what your project expects?

### skip-msys

[:octicons-tag-24: 0.1.2.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.2.0)

Restrictions: Windows systems only

Default: `false`

Command line equivalent (takes precedence): `--[no-]skip-msys` flag

Skips checking for and installing MSYS2 when stack is Setting up the
environment. This usually doesn't make sense in project-level configurations,
just in `config.yaml`.

~~~yaml
skip-msys: true
~~~

### snapshot-location-base

[:octicons-tag-24: 2.5.1](https://github.com/commercialhaskell/stack/releases/tag/v2.5.1)

Default: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/
(as set in the `pantry` library)

Command line equivalent (takes precedence): `--snapshot-location-base` option

Sets the base location of the LTS Haskell or Stackage Nightly snapshots.

For example:

~~~yaml
snapshot-location-base: https://example.com/snapshots/location/
~~~

has the following effect:

* `lts-X.Y` expands to `https://example.com/snapshots/location/lts/X/Y.yaml`
* `nightly-YYYY-MM-DD` expands to
  `https://example.com/snapshots/location/nightly/YYYY/M/D.yaml`

This key is convenient in setups that restrict access to GitHub, for instance
closed corporate setups. In this setting, it is common for the development
environment to have general access to the internet, but not for testing/building
environments. To avoid the firewall, one can run a local snapshots mirror and
then use a custom `snapshot-location-base` in the closed environments only.

### stack-colors

Command line equivalent (takes precedence): `--stack-colors` option

Stack uses styles to format some of its output. The default styles do not work
well with every terminal theme. This option specifies Stack's output styles,
allowing new styles to replace the defaults. The option is used as
`stack-colors: <STYLES>`, where `<STYLES>` is a colon-delimited sequence of
key=value, 'key' is a style name and 'value' is a semicolon-delimited list of
'ANSI' SGR (Select Graphic Rendition) control codes (in decimal). Use the
command `stack ls stack-colors --basic` to see the current sequence.

The 'ANSI' standards refer to (1) standard ECMA-48 'Control Functions for Coded
Character Sets' (5th edition, 1991); (2) extensions in ITU-T Recommendation
(previously CCITT Recommendation) T.416 (03/93) 'Information Technology – Open
Document Architecture (ODA) and Interchange Format: Character Content
Architectures' (also published as ISO/IEC International Standard 8613-6); and
(3) further extensions used by 'XTerm', a terminal emulator for the X Window
System. The 'ANSI' SGR codes are described in a
[Wikipedia article](http://en.wikipedia.org/wiki/ANSI_escape_code)
and those codes supported on current versions of Windows in
[Microsoft's documentation](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences).

For example, users of the popular
[Solarized Dark](https://ethanschoonover.com/solarized/)
terminal theme might wish to set the styles as follows:

~~~yaml
stack-colors: error=31:good=32:shell=35:dir=34:recommendation=32:target=95:module=35:package-component=95:secondary=92:highlight=32
~~~
In respect of styles used in verbose output, some of that output occurs before
the configuration file is processed.

(The British English spelling (colour) is also accepted. In YAML configuration
files, the American spelling is the alternative that has priority.)

### stack-developer-mode

[:octicons-tag-24: 2.3.3](https://github.com/commercialhaskell/stack/releases/tag/v2.3.3)

Default (official distributed binaries): `false`

Default (built from source): `true`

Turns on a mode where some messages are printed at WARN level instead of DEBUG
level, especially useful for developers of Stack itself.

~~~yaml
stack-developer-mode: false
~~~

### system-ghc

Default: `false`, unless the [Docker](docker_integration.md) or
[Nix](nix_integration.md) integration is enabled.

Command line equivalent (takes precedence): `--[no-]system-ghc` flag

Enables or disables using the GHC available on the PATH. (Make sure PATH is
explicit, i.e., don't use ~.) Useful to enable if you want to save the time,
bandwidth or storage space needed to setup an isolated GHC.

In a Nix-enabled configuration, Stack is incompatible with `system-ghc: false`.

~~~yaml
# Turn on system GHC
system-ghc: true
~~~

### templates

Command line equivalent (takes precedence): `stack new --param <key>:<value>`
(or `-p`) option

Templates used with `stack new` have a number of parameters that affect the
generated code. These can be set for all new projects you create. The result of
them can be observed in the generated LICENSE and Cabal files. The value for all
of these parameters must be strings.

The parameters are: `author-email`, `author-name`, `category`, `copyright`,
`year` and `github-username`.

* _author-email_ - sets the `maintainer` property in Cabal
* _author-name_ - sets the `author` property in Cabal and the name used in
  LICENSE
* _category_ - sets the `category` property in Cabal. This is used in Hackage.
  For examples of categories see
  [Packages by category](https://hackage.haskell.org/packages/). It makes sense
  for `category` to be set on a per project basis because it is uncommon for all
  projects a user creates to belong to the same category. The category can be
  set per project by passing `-p "category:value"` to the `stack new` command.
* _copyright_ - sets the `copyright` property in Cabal. It is typically the
  name of the holder of the copyright on the package and the year(s) from which
  copyright is claimed. For example: `Copyright (c) 2006-2007 Joe Bloggs`
* _year_ - if `copyright` is not specified, `year` and `author-name` are used
  to generate the copyright property in Cabal. If `year` is not specified, it
  defaults to the current year.
* _github-username_ - used to generate `homepage` and `source-repository` in
  Cabal. For instance `github-username: myusername` and
  `stack new my-project new-template` would result:

~~~yaml
homepage: http://github.com/myusername/my-project#readme

source-repository head
  type: git
  location: https://github.com/myusername/my-project
~~~

These properties can be set in `config.yaml` as follows:
~~~yaml
templates:
  params:
    author-name: Your Name
    author-email: youremail@example.com
    category: Your Projects Category
    copyright: 'Copyright (c) 2022 Your Name'
    github-username: yourusername
~~~

Additionally, `stack new` can automatically initialize source control
repositories in the directories it creates.  Source control tools can be
specified with the `scm-init` option. At the moment, only `git` is supported.

~~~yaml
templates:
  scm-init: git
~~~

### urls

Default:

~~~yaml
urls:
  latest-snapshot: https://www.stackage.org/download/snapshots.json
~~~

Customize the URLs where Stack looks for snapshot build plans.

### with-gcc

Command line equivalent (takes precedence): `--with-gcc` option

Specify a path to GCC explicitly, rather than relying on the normal path
resolution.

~~~yaml
with-gcc: /usr/local/bin/gcc-5
~~~

### with-hpack

Command line equivalent (takes precedence): `--with-hpack` option

Use an [Hpack](https://github.com/sol/hpack) executable, rather than Stack's
in-built version of the Hpack functionality.

~~~yaml
with-hpack: /usr/local/bin/hpack
~~~

### work-dir

[:octicons-tag-24: 0.1.10.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.10.0)

Default: `.stack-work`

Command line equivalent (takes precedence): `--work-dir` option

Environment variable alternative (lowest precedence): `STACK_WORK`

`work-dir` (or the contents of `STACK_WORK`) specifies the relative path of
Stack's 'work' directory.

## Customisation scripts

### GHC installation customisation

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

On Unix-like operating systems and Windows, Stack's installation procedure can
be fully customised by placing a `sh` shell script (a 'hook') in the Stack root
directory at `hooks/ghc-install.sh`. On Unix-like operating systems, the script
file must be made executable. The script is run by the `sh` application (which
is provided by MSYS2 on Windows).

The script **must** return an exit code of `0` and the standard output **must**
be the absolute path to the GHC binary that was installed. Otherwise Stack will
ignore the script and possibly fall back to its own installation procedure.

The script is not run when `system-ghc: true`.

When `install-ghc: false`, the script is still run, which allows you to ensure
that only your script will install GHC and Stack won't default to its own
installation logic, even when the script fails.

The following environment variables are always available to the script:

* `HOOK_GHC_TYPE = "bindist" | "git" | "ghcjs"`

For "bindist", additional variables are:

* `HOOK_GHC_VERSION = <ver>`

For "git", additional variables are:

* `HOOK_GHC_COMMIT = <commit>`
* `HOOK_GHC_FLAVOR = <flavor>`

For "ghcjs", additional variables are:

* `HOOK_GHC_VERSION = <ver>`
* `HOOK_GHCJS_VERSION = <ver>`

An example script is:

~~~sh
#!/bin/sh

set -eu

case $HOOK_GHC_TYPE in
	bindist)
		# install GHC here, not printing to stdout, e.g.:
		#   command install $HOOK_GHC_VERSION >/dev/null
		;;
	git)
		>&2 echo "Hook doesn't support installing from source"
		exit 1
		;;
	*)
		>&2 echo "Unsupported GHC installation type: $HOOK_GHC_TYPE"
		exit 2
		;;
esac

echo "location/to/ghc/executable"
~~~

If the following script is installed by GHCup, GHCup makes use of it, so that if
Stack needs a version of GHC, GHCup takes over obtaining and installing that
version:

~~~sh
#!/bin/sh

set -eu

case $HOOK_GHC_TYPE in
    bindist)
        ghcdir=$(ghcup whereis --directory ghc "$HOOK_GHC_VERSION" || ghcup run --ghc "$HOOK_GHC_VERSION" --install) || exit 3
        printf "%s/ghc" "${ghcdir}"
        ;;
    git)
        # TODO: should be somewhat possible
        >&2 echo "Hook doesn't support installing from source"
        exit 1
        ;;
    *)
        >&2 echo "Unsupported GHC installation type: $HOOK_GHC_TYPE"
        exit 2
        ;;
esac
~~~
