# Advent of Code 2021 Solutions in Haskell

## Usage

### Install stack

Go to [docs.haskellstack.org](https://docs.haskellstack.org/en/stable/README/) and install it for your OS.

### Run solutions

```
stack run aoc2021-solutions [day [part]]
```

Optional arguments:  
`day`: number between 1 and max solved problems  
`part`: number between 1 and 2

No arguments will print the solutions to all problems.

## Create a new day template

### Install python3.8

Install `python3.8` from your favourite package manager.

### Create templates

```
./newday day name
```

Positional arguments:
`day`: day number as an integer
`name`: name of new input file

It will create a new haskel module for the specified day, as well as an empty input and sample file. Additionally, it
updates the main file directly with the new module. directly to the main file.

### Troubleshoot

If there are problems with access rights try:

```
chmod +x newday
```
