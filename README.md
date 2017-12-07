# comms [![Build Status](https://api.travis-ci.org/aputlock/comms.svg?branch=master)](https://travis-ci.org/aputlock/comms)
Sending e-mail over the Ethereum blockchain. Hopefully, covertly.
## Installation
Clone this repo and then with [stack](https://www.haskellstack.org/) do ```$ stack install comms```

## Dependencies
- Ethereum node (Parity, Geth, etc)
	- Note: can also run with hosted nodes such as Infura, but this allows for traffic analysis by ISP
- Email client (Thunderbird)
    - Note: may work with other clients, but only Thunderbird is tested

## Usage

Assuming that `stack` has installed comms to somewhere on your `PATH` you should
be able to run:
```sh
$ comms -h
```

    comms [COMMAND] ... [OPTIONS]
    	Decentralized email server.

	Common flags:
    	-h --help             Display help message
        -V --version          Print version information
        --numeric-version  Print just the version number

	comms [run] [OPTIONS]
  		Runs the servers.

  		-d --debug            Print debugging info about the server

	comms import [OPTIONS] EMAIL HASH
  		Import a contact into a local only address book.

	comms publish [OPTIONS]
  		Publish contact card onto the public transaction log.
