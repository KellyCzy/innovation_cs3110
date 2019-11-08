## Overview
We are planning to build an Ocaml project which models the board game Innovation with four users, and display the board in the terminal.

#### To Begin:

Each player is dealt two cards, melded and put onto the board, decide turn order of the users.
Cards for ten ages are stacked on the board, users start taking cards from age 1.

#### To Play:

Each player has hand cards and board cards.
Hand cards are the user’s cards not yet put on the board.
Board cards are the cards splayed on the board.
Board cards are stacked together according to color, each stack could splay left, right, up, or doesn’t splay at all.
Could score a card, put under the left of the rule card.
Could achieve, put under the right of the rule card.
Five achievements cards could be achieved anytime when satisfied the requirements

#### Game Rules:

* Meld: put card from your hand to your board, on top of the stack of matching color. Continue a spaly if one is present.
* Draw: Take a card of value equal to your highest top card from the supply piels. If empty, draw from the next higher pile.
* Achieve: To claim, must have a score of at least 5x age number in points, and a top card of equal or higher value. Points are not spent, you keep them.
* Dogma/take action: Pick a top card on your board, and execute each effect on it in order. Effects are mandatory unless “You may” precedes them.
* I Demand effects are executed by each player with fewer of the features icon than you, going clockwise. Read the effect out loud to them.
* Opponents execute non-demand effects before you, if they have as many or more of the featured icon, going clockwise.
* If any opponent shared a non-demand effect, and anything happened, take a single free Draw action at the conclusion of your Dogma action.

## Helpful Resource

* A [video tutorial] (https://www.youtube.com/watch?v=Ccoe-KPbQ-g) of the board game Innovation we're trying to implement. 
* [Rule book] (http://asmadigames.com/files/InnovationRules.pdf) of the game

## Setting Up
`git clone https://github.com/QuintessaQ/innovation_cs3110.git`

## Build
* `make build` to build the game
* `make play` to start playing

## Commands
* `draw [era_num]`: draw a card from era `era_num`
* `meld [hand_idx]`: meld a card with index `hand_idx` from your hand cards
* `board [player_idx]`: display the player `player_idx`'s board cards
* More features upcoming!

## One Example of How to Play
* `make play`
* `innov.json`
* `draw 1` to draw an card from era 1
* `hand 0` to display the handcard of player 0
* `meld 1` to meld the frist card from hand
* `board 0` to display the handcard of player 0
* `draw 1`
* `meld 1`
* ...