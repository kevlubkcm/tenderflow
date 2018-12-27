# tenderflow

A visualizer for Tendermint consensus

## Overview

Tendermint consensus is [simple](https://tendermint.com/docs/introduction/what-is-tendermint.html#consensus-overview), shouldn't it have a simple visualization?

## Current Features

1. Validators are shown as red circles. Mouse over for the validator's address
2. Current proposer is a green circle.
3. On receiving a complete proposal, a red block is sent from the proposer to the center
4. On receiving a pre-vote, a blue circle is sent from the validator to the block
5. On receiving a pre-commit, a green circle is sent from the validator to the block
6. On successful commit, the proposed block turns green and is moved to the blockchain


## Known Issues/TODO

1. Validator set does not auto update. If there is a change, everything breaks. You must refresh the page
2. Pre-votes/commits for nil blocks are not distinguished from good pre-votes/commits

## Development

To get an interactive development environment run:

    lein fig:build

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

	lein clean

To create a production build run:

	lein clean
	lein fig:min


## License

Copyright © 2018 Kevin Lu

Distributed under the MIT License
