# haskbox

Unix utilities in Haskell (busybox-style).

## Build

```bash
cabal build
```

## Install

```bash
cabal install --installdir=$HOME/.local/bin
```

## Usage

```bash
haskbox <command> [args...]
```

### Available commands

| Command | Description |
|---------|-------------|
| cat | Concatenate and print files |

Run `haskbox <command> --help` for command-specific help.

The rest of the commands will be added as they are implemented.

## Benchmarks

macOS, Apple M4 Pro (cat command):

| Test | haskbox | gcat | Ratio |
|------|---------|------|-------|
| Empty file | 16.1ms | 1.9ms | 8.5x |
| 100MB file | 17.7ms | 8.3ms | 2.1x |

The performance gap is due to GHC's runtime system initialization (~15ms overhead). This is fundamental to how GHC works; it must set up the garbage collector, scheduler, and I/O subsystem before any Haskell code runs. GNU cat, written in C, has no such runtime. Once running, throughput is comparable.

## TODO

<details>
<summary>Coreutils implementation status (1/74)</summary>

| Command   | Status | Description                         |
|-----------|--------|-------------------------------------|
| basename  | ⬚      | Strip directory from filename       |
| cat       | ✅      | Concatenate and print files         |
| chgrp     | ⬚      | Change group ownership              |
| chmod     | ⬚      | Change file permissions             |
| chown     | ⬚      | Change file owner                   |
| chroot    | ⬚      | Run command with different root     |
| cksum     | ⬚      | Checksum and count bytes            |
| comm      | ⬚      | Compare sorted files                |
| cp        | ⬚      | Copy files                          |
| cut       | ⬚      | Remove sections from lines          |
| date      | ⬚      | Print or set date/time              |
| dd        | ⬚      | Convert and copy files              |
| df        | ⬚      | Report filesystem disk space        |
| dirname   | ⬚      | Strip filename from path            |
| dos2unix  | ⬚      | Convert line endings                |
| du        | ⬚      | Estimate file space usage           |
| echo      | ⬚      | Print arguments                     |
| env       | ⬚      | Run program in modified environment |
| expand    | ⬚      | Convert tabs to spaces              |
| expr      | ⬚      | Evaluate expressions                |
| factor    | ⬚      | Factor integers                     |
| false     | ⬚      | Return false                        |
| fold      | ⬚      | Wrap lines to fit width             |
| head      | ⬚      | Output first part of files          |
| hostid    | ⬚      | Print host identifier               |
| id        | ⬚      | Print user/group IDs                |
| install   | ⬚      | Copy files and set attributes       |
| link      | ⬚      | Create hard link                    |
| ln        | ⬚      | Create links                        |
| logname   | ⬚      | Print login name                    |
| ls        | ⬚      | List directory contents             |
| md5sum    | ⬚      | Compute MD5 checksum                |
| mkdir     | ⬚      | Create directories                  |
| mkfifo    | ⬚      | Create named pipes                  |
| mknod     | ⬚      | Create special files                |
| mktemp    | ⬚      | Create temporary file               |
| mv        | ⬚      | Move/rename files                   |
| nice      | ⬚      | Run with modified priority          |
| nl        | ⬚      | Number lines                        |
| nohup     | ⬚      | Run immune to hangups               |
| nproc     | ⬚      | Print number of processors          |
| od        | ⬚      | Dump files in octal                 |
| paste     | ⬚      | Merge lines of files                |
| printenv  | ⬚      | Print environment variables         |
| printf    | ⬚      | Format and print data               |
| pwd       | ⬚      | Print working directory             |
| readlink  | ⬚      | Print resolved symlink              |
| realpath  | ⬚      | Print resolved path                 |
| rm        | ⬚      | Remove files                        |
| rmdir     | ⬚      | Remove directories                  |
| seq       | ⬚      | Print number sequence               |
| sha1sum   | ⬚      | Compute SHA1 checksum               |
| sha256sum | ⬚      | Compute SHA256 checksum             |
| sha512sum | ⬚      | Compute SHA512 checksum             |
| shred     | ⬚      | Securely delete files               |
| shuf      | ⬚      | Shuffle lines                       |
| sleep     | ⬚      | Delay execution                     |
| sort      | ⬚      | Sort lines                          |
| split     | ⬚      | Split file into pieces              |
| stat      | ⬚      | Display file status                 |
| stty      | ⬚      | Change terminal settings            |
| sum       | ⬚      | Checksum and count blocks           |
| sync      | ⬚      | Sync filesystems                    |
| tac       | ⬚      | Reverse cat                         |
| tail      | ⬚      | Output last part of files           |
| tee       | ⬚      | Copy stdin to files                 |
| test      | ⬚      | Check file types and compare        |
| timeout   | ⬚      | Run with time limit                 |
| touch     | ⬚      | Change file timestamps              |
| tr        | ⬚      | Translate characters                |
| true      | ⬚      | Return true                         |
| truncate  | ⬚      | Shrink/extend file size             |
| tsort     | ⬚      | Topological sort                    |
| tty       | ⬚      | Print terminal name                 |
| uname     | ⬚      | Print system info                   |
| unexpand  | ⬚      | Convert spaces to tabs              |
| uniq      | ⬚      | Filter duplicate lines              |
| unlink    | ⬚      | Remove file                         |
| usleep    | ⬚      | Sleep in microseconds               |
| uudecode  | ⬚      | Decode uuencoded file               |
| uuencode  | ⬚      | Encode binary file                  |
| wc        | ⬚      | Count lines/words/bytes             |
| who       | ⬚      | Show logged in users                |
| whoami    | ⬚      | Print effective user                |
| yes       | ⬚      | Output string repeatedly            |


</details>

<details>
<summary>Moreutils implementation status (0/15)</summary>

| Command  | Status | Description                             |
|----------|--------|-----------------------------------------|
| chronic  | ⬚      | Run command quietly unless it fails     |
| combine  | ⬚      | Combine lines using boolean operations  |
| errno    | ⬚      | Look up errno names and descriptions    |
| ifdata   | ⬚      | Get network interface info              |
| ifne     | ⬚      | Run command if stdin is not empty       |
| isutf8   | ⬚      | Check if input is valid UTF-8           |
| mispipe  | ⬚      | Pipe commands, return first exit status |
| parallel | ⬚      | Run multiple jobs at once               |
| pee      | ⬚      | Tee stdin to pipes                      |
| sponge   | ⬚      | Soak up stdin and write to file         |
| ts       | ⬚      | Timestamp stdin                         |
| vidir    | ⬚      | Edit directory in text editor           |
| vipe     | ⬚      | Insert text editor into pipe            |
| zrun     | ⬚      | Auto-uncompress arguments to command    |

</details>
