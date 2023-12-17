#!/usr/bin/env python3

from collections import defaultdict
from functools import cmp_to_key
import sys

# cards
WILD = 1
TWO = 2
THREE = 3
FOUR = 4
FIVE = 5
SIX = 6
SEVEN = 7
EIGHT = 8
NINE = 9
TEN = 10
QUEEN = 12
KING = 13
ACE = 14

# hand ranks
HIGH_CARD=0
ONE_PAIR=1
TWO_PAIRS=2
THREE_OF_A_KIND=3
FULL_HOUSE=4
FOUR_OF_A_KIND=5
FIVE_OF_A_KIND=6

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def parse_input(contents):
    result = []
    cards = {
        '2': TWO,
        '3': THREE,
        '4': FOUR,
        '5': FIVE,
        '6': SIX,
        '7': SEVEN,
        '8': EIGHT,
        '9': NINE,
        'T': TEN,
        'J': WILD,
        'Q': QUEEN,
        'K': KING,
        'A': ACE,
    }
    for line in contents.splitlines():
        hand_part, bid_part = line.split()
        hand = list(map(lambda c: cards[c], hand_part))
        bid = int(bid_part)
        result.append((hand, bid))
    return result

def rank_hand(hand):
    histo = defaultdict(int)
    for card in hand:
        histo[card] += 1
    if len(histo.keys()) == 1:
        return FIVE_OF_A_KIND
    elif len(histo.keys()) == 2:
        counts = sorted(list(histo.values()))
        if counts == [1, 4]:
            if WILD in histo.keys():
                return FIVE_OF_A_KIND
            else:
                return FOUR_OF_A_KIND
        elif counts == [2, 3]:
            if WILD in histo.keys():
                return FIVE_OF_A_KIND
            else:
                return FULL_HOUSE
    elif len(histo.keys()) == 3:
        counts = sorted(list(histo.values()))
        if counts == [1, 1, 3]:
            if WILD in histo.keys():
                return FOUR_OF_A_KIND
            else:
                return THREE_OF_A_KIND
        elif counts == [1, 2, 2]:
            if WILD in histo.keys():
                if histo[WILD] == 1:
                    return FULL_HOUSE
                else:
                    return FOUR_OF_A_KIND
            else:
                return TWO_PAIRS
    elif len(histo.keys()) == 4:
        if WILD in histo.keys():
            return THREE_OF_A_KIND
        else:
            return ONE_PAIR
    elif WILD in histo.keys():
        return ONE_PAIR
    else:
        return HIGH_CARD

def compare_hands(first, second):
    first_rank = rank_hand(first[0])
    second_rank = rank_hand(second[0])
    if first_rank != second_rank:
        return first_rank - second_rank
    else:
        for pairs in zip(first[0], second[0]):
            if pairs[0] != pairs[1]:
                return pairs[0] - pairs[1]
    return 0

def process(contents):
    hands_with_bids = parse_input(contents)
    hands_with_bids.sort(key=cmp_to_key(compare_hands))
    sum = 0
    for (i, hand_with_bid) in enumerate(hands_with_bids):
        sum += (i + 1) * hand_with_bid[1]
    return sum

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    try:
        infile = open(filename)
        contents = infile.read()
        result = process(contents)
        print(f'result = {result}')
    except IOError:
        print(f'read of input file "{filename}" failed.')
        sys.exit(1)

if __name__ == '__main__':
    main()
