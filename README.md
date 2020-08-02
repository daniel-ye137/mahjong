# Mahjong
Created by Gracie Jing, Daniel Ye, and Wanxing Lu for CS 3110 Spring 2020

Additional Collaborators: Jeff Van

## Overview
We created the traditional Chinese game Mahjong using OCaml and a terminal-based UI system. Mahjong is a turn-based game requiring the 4 players to draw and discard tiles until they complete a legal hand. There are additional actions such as “bumping” and “eating” pieces discarded by other players. In this version, you play against 3 robot players.

## Rules
### Initialization
- 144 tiles in total
- 4 of each suit
- Suits: 
  - Stick: 1-9
  - Disc: 1-9
  - Tens Thousands: 1-9
  - Winds: East, West, North, South
  - Red China
  - Green Wealth
  - White Painting
- Every player starts out with 13 tiles

### Formations
- 4 of a kind
  - 4 of the same tile, ie. 3, 3, 3, of Disc
- 3 of a kind
  - 3 in a row of the same suit, i.e. 3, 4, 5 of Disc
  - 3 of the same tile, i.e. 3, 3, 3 of Disc
- 2 of a kind
  - Pair of tiles, i.e. 3, 3 of Disc

### Turn
1. Player draws a tile

On Drawing, the player can:
  - **Gang:** If the drawn tile creates a set of 4 of a kind, the player can "gang" to display this set. The player then draws a tile from the end of the deck to replace this tile.
  - **Win:** If the drawn tile allows the player to create a winning hand, they can win.

2. Player either wins or discards a tile of type T

On Discard, other players can:
  - **Bump:** Any other player who has two of T can “bump” the T just discarded. They lay these tiles face up on the board so everyone else can see them, and then they discard a tile of their choosing. Normal play begins with the player directly after them. “Bumping” takes precedence over “eating.”
  - **Eat:** If the player directly after the current player can create a “3-in-a-row” with the tile T just discarded (for example, player 1 discards 3 of Disc, and player 2 has 3, 5 of Disc) then that player can “eat” the T just discarded. They lay these tiles face up on the board so everyone can see them, and then they discard a tile of their choosing. Normal play begins with the player directly after them.
  - **Gang:** Any other player who has three of T can "gang" the T just discarded. They lay these tiles face up on the board so everyone can see them, and then they discard a tile of their choosing. Normal play begins with the player directly after them. “Ganging” takes precedence over “eating.”
  - **Win:** If any player is waiting on one tile to win the game, they can take a discarded tile regardless of the position of the player who discarded the tile to win.

### Win Condition
4 sets of 3 of a kind + 1 set of 2 of a kind (sets of 4 of a kind are treated as sets of 3 of a kind)

## Color Scheme
**Green:** Indicates the start or end of the game as well as the start of a new player's turn.

**Yellow:** Indicates an action.

**Red:** Indicates an error.

## Final Thoughts
We hope you enjoy. Have fun! :D
