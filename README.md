# comms [![Build Status](https://travis-ci.com/aputlock/comms.svg?token=su2fqvpmA5RE6UXKS1zp&branch=master)](https://travis-ci.com/aputlock/comms)
Sending e-mail over the Ethereum blockchain. Hopefully, covertly.
## Installation
Clone this repo and then with [stack](https://www.haskellstack.org/) do ```$ stack install comms```

## Usage

Assuming that `stack` has installed comms to somewhere on your `PATH` you should
be able to run:
```sh
$ comms config.json
```
where `config.json` is a filepath.
    Comms [OPTIONS] [FILE]
        Decentralized email server.

    Common flags:
        -d --debug            Print debugging info about the server
	    -h --help             Display help message
	        -V --version          Print version information
		       --numeric-version  Print just the version number

    Expects the location of the configuration file as an argument to the program.