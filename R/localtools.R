# Provisioning for the local agena.ai engine.
#
# Until now local_api_clone() + local_api_compile() fetched the API's *source* from
# public GitHub and built it with Maven, which meant every user needed git, Maven and a
# JDK on their PATH to produce an artifact we can publish once. The API source is now
# closed and published as a pre-built runtime bundle instead, so this file downloads
# that bundle and the two things it deliberately does not carry: a Java runtime, and the
# Cryptlex licensing components.
#
# Everything is installed under ~/.agena.ai/tools, the same directory agena.ai Explorer
# uses, so a machine with either installed already has most of what the other needs.
# Nothing is written to the working directory: the old code cloned ./api next to
# whatever the user was doing and setwd()-ed into it to run Maven.

.agena_tools_dir <- function() {
  override <- Sys.getenv("AGENA_TOOLS_DIR")
  if (nzchar(override)) return(override)
  file.path(path.expand("~"), ".agena.ai", "tools")
}

.agena_api_dir <- function() file.path(.agena_tools_dir(), "api")
# Java major the installed api jar asks for, or the default when it says nothing.
#
# Read from the jar's Require-Java manifest attribute. The jar is the only thing that
# knows which runtime it was built and tested for, so it says so and clients obey --
# rather than each client pinning a version of its own and hoping they agree.
.agena_required_java_major <- function(jar = NULL) {
  if (is.null(jar)) jar <- .agena_jar_path()
  if (is.null(jar) || !file.exists(jar)) return(.AGENA_DEFAULT_JAVA_MAJOR)

  major <- tryCatch({
    staging <- tempfile("agena-manifest-")
    dir.create(staging)
    on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(jar, files = "META-INF/MANIFEST.MF", exdir = staging)
    lines <- readLines(file.path(staging, "META-INF", "MANIFEST.MF"), warn = FALSE)
    hit <- grep("^Require-Java:[[:space:]]*[0-9]+", lines, value = TRUE)
    if (length(hit) == 0) {
      .AGENA_DEFAULT_JAVA_MAJOR
    } else {
      # regmatches rather than sub with a backreference: the replacement form returned
      # an empty string on the R build this was developed against, which would have
      # coerced to NA and quietly fallen back to the default.
      as.integer(regmatches(hit[1], regexpr("[0-9]+", hit[1])))
    }
  }, error = function(e) {
    # An unreadable manifest is not worth failing over: the default is a runtime this
    # jar can load, since the floor has only ever gone up from 8.
    .AGENA_DEFAULT_JAVA_MAJOR
  })
  if (is.na(major)) .AGENA_DEFAULT_JAVA_MAJOR else major
}

#' @noRd
.agena_jre_build <- function(major) {
  key <- as.character(major)
  # Membership is checked before subsetting: `[[` on a name a vector does not have
  # raises "subscript out of bounds", which would replace the explanation below with
  # something the caller cannot act on.
  if (!key %in% names(.AGENA_JRE_BUILDS)) {
    stop("The agena.ai API requires Java ", major, ", which this version of the ",
         "agena.ai package does not know how to install. Update the package, or upload ",
         "that JRE to ", .AGENA_JRE_BASE_URL, "/", major, "/ and add it to ",
         ".AGENA_JRE_BUILDS.",
         call. = FALSE)
  }
  .AGENA_JRE_BUILDS[[key]]
}

# Version-stamped, so switching Java majors actually takes effect on a machine that
# already has another one. Unstamped, "is Java present" answered yes for any Java once
# unpacked here, so a change of runtime would have been silently ignored on every
# existing install -- reaching new users only.
.agena_java_dir <- function(major = NULL) {
  if (is.null(major)) major <- .agena_required_java_major()
  file.path(.agena_tools_dir(), "jre", .agena_jre_build(major))
}

.AGENA_REPO_RELEASE <- "https://mvn.agenarisk.com/repository/internal"
.AGENA_REPO_SNAPSHOT <- "https://mvn.agenarisk.com/repository/snapshots"
.AGENA_GROUP_ID <- "com.agenarisk"
.AGENA_ARTIFACT_ID <- "com.agenarisk.api"

.AGENA_LICENSING_URL <- "https://resources.agena.ai/download/archive/lib-v3.44.0.zip"
.AGENA_PRODUCT_URL <- "https://resources.agena.ai/products/v3/developer/product.json"
.AGENA_JRE_BASE_URL <- "https://resources.agena.ai/tools/jre"

# Exact JRE build published on resources.agena.ai for each Java major.
#
# Which major to use is not this package's decision -- it is the api jar's, read from its
# manifest (see .agena_required_java_major). This table only says which build of that
# major we ship. A required major missing here is an error naming the version, rather
# than a silent fallback: running the engine on a runtime it did not ask for is the
# failure this mechanism exists to prevent.
.AGENA_JRE_BUILDS <- c("21" = "21.0.12.1_1", "25" = "25.0.4.1_1")

# Archives re-hosted from another vendor because Adoptium publishes none.
#
# Keyed <major>-<os>-<arch>. Temurin has no Windows ARM64 build for 25, though it does
# for 21; Azul ships that platform at the same OpenJDK patch level, so the substitution
# costs nothing and keeps every user on one engine version. The real filename is kept
# rather than renamed to look like a Temurin build -- the composed name says "hotspot",
# and it would be a lie. Layout is not a concern: the extractor here strips a single
# wrapper directory, which is what every vendor ships.
.AGENA_JRE_VENDOR_OVERRIDES <- c(
  "25-windows-aarch64" = "zulu25.36.205-ca-jre25.0.4.1-win_aarch64.zip"
)

# The Java major used when the jar states no requirement.
#
# Every jar built before Require-Java existed falls here. 21 rather than the bytecode
# target: the jar is compiled with release 8, so its class-file version says "any JVM
# from 8 up", which is a floor and not a preference -- honouring it literally would
# install a Java 8 JRE nobody has tested against.
.AGENA_DEFAULT_JAVA_MAJOR <- 21L

# How many API versions to keep: enough to roll back, and to hold a snapshot beside a
# release. Past that they are only disk.
.AGENA_KEEP_VERSIONS <- 3

# Versions below this major use the legacy fractional-minor scheme.
.AGENA_SEMVER_FROM_MAJOR <- 2

#' Sort key that is correct for both API version schemes
#'
#' Before 2.0.0 the API used major.minor where the minor was a three-digit *fraction*:
#' 1.03 meant 1.030, and the series ran 0.1 ... 1.043. Compared component-wise, 0.98
#' reads as c(0, 98) and 0.971 as c(0, 971), making 0.971 look newer -- but 0.98 meant
#' 0.980 and is in fact newer. The published history has 156 such pairs, so resolving
#' "the latest version" naively really does pick the wrong artifact.
#'
#' Padding a two-component minor back out to the three digits it was written as fixes
#' it. The switch to semver was made at a major boundary on purpose, so the two schemes
#' only ever need telling apart, never reconciling: every 2.x.y outranks every 1.x
#' under either reading.
#' @noRd
.agena_version_key <- function(version) {
  raw <- strsplit(sub("-SNAPSHOT$", "", version), ".", fixed = TRUE)[[1]]
  out <- suppressWarnings(as.numeric(raw))
  out[is.na(out)] <- 0
  if (length(raw) == 2 && out[1] < .AGENA_SEMVER_FROM_MAJOR) {
    padded <- if (nchar(raw[2]) < 3) substr(paste0(raw[2], "000"), 1, 3) else raw[2]
    out[2] <- suppressWarnings(as.numeric(padded))
    if (is.na(out[2])) out[2] <- 0
  }
  out
}

#' Compare two version strings; negative if a < b, positive if a > b, 0 if equal
#' @noRd
.agena_version_compare <- function(a, b) {
  ka <- .agena_version_key(a)
  kb <- .agena_version_key(b)
  n <- max(length(ka), length(kb))
  ka <- c(ka, rep(0, n - length(ka)))
  kb <- c(kb, rep(0, n - length(kb)))
  difference <- (ka - kb)[(ka - kb) != 0]
  if (length(difference) == 0) 0 else difference[1]
}

#' @noRd
.agena_version_max <- function(versions) {
  best <- versions[1]
  for (version in versions) {
    if (.agena_version_compare(version, best) > 0) best <- version
  }
  best
}

#' Version strings ordered newest first
#' @noRd
.agena_version_sort_desc <- function(versions) {
  if (length(versions) < 2) return(versions)
  ranks <- vapply(versions, function(v) {
    sum(vapply(versions, function(o) .agena_version_compare(v, o) < 0, logical(1)))
  }, numeric(1))
  versions[order(ranks)]
}

#' @noRd
.agena_repo_base <- function(channel) {
  if (!channel %in% c("release", "snapshot")) {
    stop("channel must be 'release' or 'snapshot'")
  }
  repo <- if (channel == "snapshot") .AGENA_REPO_SNAPSHOT else .AGENA_REPO_RELEASE
  paste0(repo, "/", gsub(".", "/", .AGENA_GROUP_ID, fixed = TRUE), "/", .AGENA_ARTIFACT_ID)
}

#' Fetch a small text resource
#'
#' Base R rather than httr, even though the package already imports httr for the cloud
#' functions: provisioning is what runs before anything else works, so it depends on
#' `utils` alone and cannot be broken by a problem in a package the user has yet to
#' need.
#' @noRd
.agena_get_text <- function(url) {
  connection <- base::url(url, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste(readLines(connection, warn = FALSE), collapse = "
")
}

#' Highest version published on a channel
#'
#' Deliberately ignores <latest> in the metadata: Archiva leaves it stale -- it still
#' names a version from years ago -- while <versions> is maintained. Maven's own tooling
#' distrusts <latest> for the same reason.
#' @noRd
.agena_resolve_version <- function(channel) {
  xml <- .agena_get_text(paste0(.agena_repo_base(channel), "/maven-metadata.xml"))
  matches <- regmatches(xml, gregexpr("<version>[^<]+</version>", xml))[[1]]
  if (length(matches) == 0) {
    stop("No versions published on the ", channel, " channel")
  }
  .agena_version_max(trimws(gsub("</?version>", "", matches)))
}

#' File name of the runtime bundle for a version
#'
#' A release is named after its version; a snapshot is not, because Maven publishes each
#' build under a timestamped name. That name is composed from <timestamp> and
#' <buildNumber> rather than read from the more explicit <snapshotVersions> list,
#' because Archiva's repository scanner rewrites maven-metadata.xml and drops that list
#' -- so a resolver depending on it works right up until the first scan runs, then fails
#' on artifacts that are still perfectly well there.
#' @noRd
.agena_bundle_name <- function(base, version) {
  if (!grepl("-SNAPSHOT$", version)) {
    return(paste0(.AGENA_ARTIFACT_ID, "-", version, "-runtime.zip"))
  }

  xml <- .agena_get_text(paste0(base, "/", version, "/maven-metadata.xml"))
  extract <- function(tag) {
    hit <- regmatches(xml, regexpr(paste0("<", tag, ">[^<]+</", tag, ">"), xml))
    if (length(hit) == 0) return(NULL)
    gsub(paste0("</?", tag, ">"), "", hit)
  }
  timestamp <- extract("timestamp")
  build_number <- extract("buildNumber")
  if (!is.null(timestamp) && !is.null(build_number)) {
    stem <- sub("-SNAPSHOT$", "", version)
    return(paste0(.AGENA_ARTIFACT_ID, "-", stem, "-", timestamp, "-", build_number, "-runtime.zip"))
  }

  # Freshly deployed and not yet scanned: the explicit list is still present.
  blocks <- regmatches(xml, gregexpr("<snapshotVersion>.*?</snapshotVersion>", xml))[[1]]
  for (block in blocks) {
    if (grepl("<classifier>runtime</classifier>", block) &&
        grepl("<extension>zip</extension>", block)) {
      value <- regmatches(block, regexpr("<value>[^<]+</value>", block))
      if (length(value) > 0) {
        return(paste0(.AGENA_ARTIFACT_ID, "-", gsub("</?value>", "", value), "-runtime.zip"))
      }
    }
  }
  stop("No runtime bundle published for ", version)
}

#' The single top-level directory shared by every entry, or NULL
#'
#' Only a directory that contains *everything* is a wrapper. Stripping the first path
#' segment unconditionally is the tempting version and it is wrong: given a bundle whose
#' root holds a jar beside a lib/ directory, it discards the jar and flattens the
#' dependencies, while reporting success.
#' @noRd
.agena_common_root <- function(names) {
  heads <- unique(vapply(strsplit(gsub("\\\\", "/", names), "/"), `[`, character(1), 1))
  if (length(heads) != 1 || is.na(heads) || !nzchar(heads)) return(NULL)
  if (!all(startsWith(gsub("\\\\", "/", names), paste0(heads, "/")))) return(NULL)
  heads
}

#' @noRd
.agena_extract <- function(archive, target) {
  staging <- tempfile("agena-extract-")
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  if (grepl("\\.zip$", archive)) {
    utils::unzip(archive, exdir = staging)
  } else {
    # untar preserves the executable bit, which the JRE's `java` needs. unzip does not,
    # but zip archives are only used where that does not matter (Windows, and native
    # libraries that are dlopen-ed rather than executed).
    utils::untar(archive, exdir = staging)
  }

  entries <- list.files(staging, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(entries) == 0) stop("Archive ", basename(archive), " was empty")

  root <- .agena_common_root(entries)
  source_dir <- if (is.null(root)) staging else file.path(staging, root)

  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  for (entry in list.files(source_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)) {
    file.copy(entry, target, recursive = TRUE, copy.mode = TRUE)
  }
}

#' @noRd
.agena_install_archive <- function(url, target, expected, force = FALSE) {
  if (!force && length(expected) > 0 && all(file.exists(expected))) {
    return(FALSE)
  }

  unlink(target, recursive = TRUE, force = TRUE)
  destination <- file.path(tempdir(), basename(url))
  message("Downloading ", url)
  # download.file warns *and* errors on an HTTP failure; the warning duplicates the
  # message callers get from the error, so only the error is allowed through.
  suppressWarnings(utils::download.file(url, destination, mode = "wb", quiet = TRUE))
  on.exit(unlink(destination, force = TRUE), add = TRUE)

  .agena_extract(destination, target)

  missing <- expected[!file.exists(expected)]
  if (length(missing) > 0) {
    unlink(target, recursive = TRUE, force = TRUE)
    stop(basename(url), " did not contain ", paste(missing, collapse = ", "))
  }
  TRUE
}

# ---------------------------------------------------------------------------------
# Java
# ---------------------------------------------------------------------------------

#' @noRd
.agena_java_home <- function(major = NULL) {
  base <- .agena_java_dir(major)
  # A macOS JRE archive is a bundle; the runtime sits inside Contents/Home.
  if (Sys.info()[["sysname"]] == "Darwin") file.path(base, "Contents", "Home") else base
}

#' @noRd
.agena_java_path <- function(major = NULL) {
  executable <- if (.Platform$OS.type == "windows") "java.exe" else "java"
  file.path(.agena_java_home(major), "bin", executable)
}

#' @noRd
.agena_os_arch <- function() {
  machine <- tolower(Sys.info()[["machine"]])
  arch <- if (machine %in% c("arm64", "aarch64")) "aarch64" else "x64"
  system <- Sys.info()[["sysname"]]
  if (system == "Windows") {
    list(os = "windows", arch = arch, ext = "zip")
  } else if (system == "Darwin") {
    list(os = "mac", arch = arch, ext = "tar.gz")
  } else if (system == "Linux") {
    list(os = "linux", arch = arch, ext = "tar.gz")
  } else {
    stop("Unsupported platform: ", system, call. = FALSE)
  }
}

#' @noRd
.agena_jre_archive_name <- function(major) {
  target <- .agena_os_arch()
  # A vendor override wins over the composed Adoptium name, for platforms Adoptium does
  # not publish at all.
  key <- paste(major, target$os, target$arch, sep = "-")
  if (key %in% names(.AGENA_JRE_VENDOR_OVERRIDES)) {
    return(.AGENA_JRE_VENDOR_OVERRIDES[[key]])
  }
  sprintf("OpenJDK%sU-jre_%s_%s_hotspot_%s.%s", major, target$arch, target$os,
          .agena_jre_build(major), target$ext)
}

#' The machine's own java and its major version, or NULL
#'
#' A system Java is the fallback for platforms no JRE is published for, but it has to be
#' asked its version rather than assumed usable: a Java 17 cannot load a jar built for
#' 21, and taking whatever is on the PATH turns a clear setup failure into an
#' UnsupportedClassVersionError part-way through a calculation.
#' @noRd
.agena_system_java <- function() {
  found <- unname(Sys.which("java"))
  if (!nzchar(found)) return(NULL)

  major <- tryCatch({
    # java -version writes to stderr, and reports either 1.8.0_x for 8 and below or
    # 21.0.12.1 from 9 on, so a leading "1." has to be stripped before the first
    # component means the major.
    out <- suppressWarnings(system2(found, "-version", stdout = TRUE, stderr = TRUE))
    hit <- regmatches(out, regexpr('version "[0-9._]+', out))
    hit <- hit[nzchar(hit)]
    if (length(hit) == 0) {
      0L
    } else {
      raw <- sub('^version "', "", hit[1])
      parts <- strsplit(raw, ".", fixed = TRUE)[[1]]
      as.integer(if (parts[1] == "1") parts[2] else parts[1])
    }
  }, error = function(e) 0L)

  list(path = found, major = if (is.na(major)) 0L else major)
}

#' The java to run this jar with: ours if installed, else a new enough system one
#'
#' Accepted on >= rather than equality -- a newer runtime satisfies the floor, and
#' refusing Java 26 when the jar asked for 25 would be unhelpfully strict.
#' @noRd
.agena_resolve_java <- function(major = NULL) {
  if (is.null(major)) major <- .agena_required_java_major()

  ours <- tryCatch(.agena_java_path(major), error = function(e) NULL)
  if (!is.null(ours) && file.exists(ours)) return(ours)

  found <- .agena_system_java()
  if (!is.null(found) && found$major >= major) {
    message("Using the Java ", found$major, " already on this machine (", found$path, ")")
    return(found$path)
  }
  NULL
}

#' Install the Java runtime the api jar asks for
#'
#' A JRE, not a JDK: nothing is compiled at runtime any more, so the compiler was only
#' ever needed by the Maven build that no longer happens. That takes the download from
#' roughly 180 MB to 47 MB.
#' @noRd
.agena_install_java <- function(major = NULL, force = FALSE) {
  if (is.null(major)) major <- .agena_required_java_major()
  # A Java we cannot download is only a problem if the machine has no suitable one of
  # its own. Where it does, the engine runs on that instead.
  name <- tryCatch(.agena_jre_archive_name(major), error = function(e) NULL)
  if (is.null(name)) {
    found <- .agena_system_java()
    if (!is.null(found) && found$major >= major) {
      message("No Java ", major, " runtime is published for this package to install; ",
              "using the Java ", found$major, " already on this machine (",
              found$path, ")")
      return(FALSE)
    }
    stop("No Java ", major, " runtime is published for this package to install, and ",
         "this machine has no Java ", major, " or newer. Install one -- Azul Zulu and ",
         "BellSoft Liberica publish platforms Adoptium does not.", call. = FALSE)
  }
  # JREs are served per major: <base>/21/<archive>, <base>/25/<archive>. A flat
  # directory would make two majors of the same product indistinguishable by path.
  installed <- .agena_install_archive(
    paste0(.AGENA_JRE_BASE_URL, "/", major, "/", name),
    .agena_java_dir(major),
    .agena_java_path(major),
    force = force
  )
  if (installed) {
    Sys.chmod(.agena_java_path(major), "0755", use_umask = FALSE)
    message("Java ", .agena_jre_build(major), " installed to ", .agena_java_dir(major))
  }
  installed
}

# ---------------------------------------------------------------------------------
# Licensing components
# ---------------------------------------------------------------------------------

#' @noRd
.agena_licensing_version <- function() {
  # The archive name is the only place this version is recorded. Stamping the install
  # directory with it means a Cryptlex upgrade cannot half-overwrite the payload in use.
  hit <- regmatches(.AGENA_LICENSING_URL, regexpr("lib-v[0-9.]+\\.zip$", .AGENA_LICENSING_URL))
  if (length(hit) == 0) return("unversioned")
  sub("\\.zip$", "", sub("^lib-v", "", hit))
}

#' @noRd
.agena_licensing_path <- function() {
  file.path(.agena_tools_dir(), "licensing", .agena_licensing_version())
}

#' @noRd
.agena_install_licensing <- function(force = FALSE) {
  target <- .agena_licensing_path()
  if (!force && file.exists(file.path(target, "product.json"))) {
    return(FALSE)
  }

  .agena_install_archive(
    .AGENA_LICENSING_URL, target, file.path(target, "lexactivator3"), force = TRUE
  )
  # product.json is published separately from the natives archive because that archive
  # is product-agnostic and this file is precisely what is not.
  utils::download.file(
    .AGENA_PRODUCT_URL, file.path(target, "product.json"), mode = "wb", quiet = TRUE
  )
  message("Licensing components installed to ", target)
  TRUE
}

# ---------------------------------------------------------------------------------
# The API bundle
# ---------------------------------------------------------------------------------

#' @noRd
.agena_installed_version <- function() {
  pointer <- file.path(.agena_api_dir(), "current")
  if (!file.exists(pointer)) return(NULL)
  version <- trimws(paste(readLines(pointer, warn = FALSE), collapse = ""))
  if (dir.exists(file.path(.agena_api_dir(), version))) version else NULL
}

#' @noRd
.agena_jar_path <- function() {
  version <- .agena_installed_version()
  if (is.null(version)) return(NULL)
  directory <- file.path(.agena_api_dir(), version)
  jars <- list.files(directory, pattern = "\\.jar$")
  jars <- jars[!grepl("-javadoc|-sources", jars)]
  if (length(jars) == 0) return(NULL)
  file.path(directory, sort(jars)[1])
}

#' @noRd
.agena_prune_old_versions <- function(keep) {
  tryCatch({
    directories <- list.dirs(.agena_api_dir(), recursive = FALSE, full.names = FALSE)
    ordered <- .agena_version_sort_desc(directories)
    for (directory in utils::tail(ordered, -.AGENA_KEEP_VERSIONS)) {
      if (identical(directory, keep)) next
      unlink(file.path(.agena_api_dir(), directory), recursive = TRUE, force = TRUE)
      message("Removed superseded API install ", directory)
    }
  }, error = function(e) {
    # Housekeeping: failing to prune must never fail an install.
    message("Could not prune old API installs: ", conditionMessage(e))
  })
  invisible(NULL)
}

#' Which channel a version came from, judged from the version string itself
#' @noRd
.agena_channel_of <- function(version) {
  if (grepl("-SNAPSHOT$", version)) "snapshot" else "release"
}

#' Install the API runtime bundle, or confirm that one is already installed
#'
#' Does nothing when the API is present and no change was asked for -- and in that case
#' does not touch the network at all. Left to resolve the newest version every time,
#' this would upgrade the engine underneath the caller: two runs of the same script
#' could use two different engines, which is exactly what a reproducible analysis must
#' not do. An upgrade is therefore something you ask for, never something that happens
#' to you.
#' @noRd
.agena_install_api <- function(channel = NULL, version = NULL, update = FALSE,
                               force = FALSE) {
  installed <- .agena_installed_version()

  if (!is.null(installed) && !force) {
    asked <- update ||
      (!is.null(version) && !identical(version, installed)) ||
      (!is.null(channel) && !identical(.agena_channel_of(installed), channel))
    if (!asked) {
      message("Using API ", installed,
              ". Call local_api_install(update = TRUE) to check for a newer one")
      return(installed)
    }
  }

  # Stay on the channel the current install came from unless told otherwise, so an
  # update = TRUE in a developer's script does not quietly move them back to releases.
  if (is.null(channel)) {
    channel <- if (!is.null(installed)) .agena_channel_of(installed) else "release"
  }
  base <- .agena_repo_base(channel)
  if (is.null(version)) version <- .agena_resolve_version(channel)
  if (!force && identical(installed, version)) {
    message("API ", version, " is already the newest on the ", channel, " channel")
    return(version)
  }

  name <- .agena_bundle_name(base, version)
  target <- file.path(.agena_api_dir(), version)
  # Replace this version's directory only: the others are kept on purpose.
  unlink(target, recursive = TRUE, force = TRUE)
  dir.create(.agena_api_dir(), recursive = TRUE, showWarnings = FALSE)

  # A version can resolve perfectly well and still have no bundle: releases made before
  # the runtime bundle existed publish only a bare jar. Saying so beats handing back a
  # download error and a URL.
  tryCatch(
    .agena_install_archive(paste0(base, "/", version, "/", name), target,
                           character(0), force = TRUE),
    error = function(e) {
      stop("Could not download the runtime bundle for ", version, " from the ", channel,
           " channel. Releases made before this distribution format publish only a bare ",
           "jar; try local_api_install(channel = \"snapshot\"). Cause: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (length(list.files(target, pattern = "\\.jar$")) == 0) {
    unlink(target, recursive = TRUE, force = TRUE)
    stop("The bundle for ", version, " contained no jar")
  }

  writeLines(version, file.path(.agena_api_dir(), "current"))
  message("Installed API ", version, " from the ", channel, " channel")

  .agena_prune_old_versions(version)
  version
}

# ---------------------------------------------------------------------------------
# Public entry point and command construction
# ---------------------------------------------------------------------------------

#' Install the local agena.ai API
#'
#' Makes sure everything needed to run agena.ai calculations on this machine is present:
#' a Java runtime, the licensing components and the agena.ai API runtime bundle.
#' Replaces `local_api_clone()` and `local_api_compile()` -- there is no git, Maven or
#' JDK involved, and nothing is written to the working directory.
#'
#' Safe to call at the top of a script. Anything already installed is left exactly as it
#' is, no network request is made, and it returns immediately -- so a script beginning
#' with this line works on a fresh machine and stays on one engine version thereafter.
#' Upgrading is opt-in: pass `update = TRUE`.
#'
#' Components are installed under `~/.agena.ai/tools`, shared with agena.ai Explorer and
#' any other local agena.ai client, so anything already present is reused. Set the
#' environment variable `AGENA_TOOLS_DIR` to install elsewhere.
#'
#' @param channel "release" or "snapshot" for unreleased builds. Defaults to the channel
#'   the installed version came from, or "release" on a fresh machine. Naming the other
#'   channel is itself a request to switch to it
#' @param version pin an exact version instead of resolving the newest
#' @param update check the channel for a newer version and install it if there is one
#' @param force reinstall even when the components are already present
#' @returns the API version now installed, invisibly
#' @export
local_api_install <- function(channel = NULL, version = NULL, update = FALSE,
                              force = FALSE) {
  # The API comes first: the jar states which Java runtime it needs, so there is nothing
  # to resolve a JRE against until it is on disk. Downloading and unpacking the bundle
  # needs no Java itself, so the order costs nothing.
  installed <- .agena_install_api(channel = channel, version = version,
                                  update = update, force = force)
  .agena_install_java(force = force)
  .agena_install_licensing(force = force)
  invisible(installed)
}

#' @noRd
.agena_require_installed <- function() {
  jar <- .agena_jar_path()
  if (is.null(jar)) {
    stop("The local agena.ai API is not installed. Run local_api_install()",
         call. = FALSE)
  }
  java <- .agena_resolve_java()
  if (is.null(java)) {
    major <- .agena_required_java_major()
    stop("No Java ", major, " or newer is available to run the agena.ai API. Run ",
         "local_api_install(), or install a Java ", major, " runtime -- Azul Zulu and ",
         "BellSoft Liberica publish platforms Adoptium does not.", call. = FALSE)
  }
  if (!file.exists(file.path(.agena_licensing_path(), "product.json"))) {
    stop("The licensing components are not installed. Run local_api_install()",
         call. = FALSE)
  }
  list(jar = jar, java = java)
}

#' Run one engine invocation
#'
#' `java -jar` rather than `mvn exec:java`, which means no shell and so no hand quoting
#' -- the old code built one command string for PowerShell and another for sh, and every
#' model path had to survive both. It also means the product and native-library
#' directories must be passed explicitly: both default to ./lib relative to the working
#' directory, which under Maven happened to be the API project and now is not.
#' @noRd
.agena_engine <- function(args, working_directory = getwd()) {
  installed <- .agena_require_installed()
  full_args <- c(
    "-jar", shQuote(installed$jar),
    args,
    "--directoryProduct", shQuote(.agena_licensing_path()),
    "--directoryNativeLibs", shQuote(.agena_licensing_path()),
    "--directoryWorking", shQuote(working_directory)
  )
  output <- suppressWarnings(
    system2(installed$java, args = full_args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(output, "status")

  # Running headless without an Enterprise licence is refused by design. Surfaced here
  # rather than left to fail confusingly further down.
  marker <- "Only Enterprise version can run in a headless environment"
  if (any(grepl(marker, output, fixed = TRUE))) stop(marker)

  list(output = output, status = if (is.null(status)) 0L else status)
}

#' Parse a licence summary from engine output
#'
#' `activate --licenseSummary --json` prints the summary as a single JSON object on its
#' own line. The previous version split the Maven log on braces and hoped the first `{`
#' it found belonged to the summary rather than to anything else the build had printed.
#' @noRd
.agena_license_info <- function(output) {
  candidates <- trimws(output)
  candidates <- candidates[grepl("^\\{.*\\}$", candidates)]
  if (length(candidates) == 0) stop("The engine did not return a license summary")
  rjson::fromJSON(candidates[length(candidates)])
}
