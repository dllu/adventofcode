use std::cmp::Ordering;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum CardRank {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl CardRank {
    fn new(r: char) -> CardRank {
        match r {
            '2' => CardRank::Two,
            '3' => CardRank::Three,
            '4' => CardRank::Four,
            '5' => CardRank::Five,
            '6' => CardRank::Six,
            '7' => CardRank::Seven,
            '8' => CardRank::Eight,
            '9' => CardRank::Nine,
            'T' => CardRank::Ten,
            'J' => CardRank::Jack,
            'Q' => CardRank::Queen,
            'K' => CardRank::King,
            'A' => CardRank::Ace,
            _ => panic!("unexpected char"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum HandRank {
    HighCard,
    OnePair,
    TwoPairs,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn rank_hand(hand: &[CardRank]) -> HandRank {
    let mut histo: HashMap<CardRank, u32> = HashMap::new();
    for card in hand.iter() {
        histo.entry(*card).and_modify(|e| *e += 1).or_insert(1);
    }
    let mut result: HandRank = HandRank::HighCard;
    if histo.values().len() == 1 {
        result = HandRank::FiveOfAKind;
    } else if histo.values().len() == 2 {
        let mut counts: Vec<u32> = histo.values().cloned().collect();
        counts.sort();
        if counts == [1, 4] {
            result = HandRank::FourOfAKind;
        } else if counts == [2, 3] {
            result = HandRank::FullHouse;
        }
    } else if histo.values().len() == 3 {
        let mut counts: Vec<u32> = histo.values().cloned().collect();
        counts.sort();
        if counts == [1, 1, 3] {
            result = HandRank::ThreeOfAKind;
        } else if counts == [1, 2, 2] {
            result = HandRank::TwoPairs;
        }
    } else if histo.values().len() == 4 {
        result = HandRank::OnePair;
    } else {
        result = HandRank::HighCard;
    }
    result
}

fn compare_hands(first: &[CardRank], second: &[CardRank]) -> Ordering {
    let first_rank: HandRank = rank_hand(first);
    let second_rank: HandRank = rank_hand(second);
    match first_rank.cmp(&second_rank) {
        Ordering::Less => Ordering::Less,
        Ordering::Greater => Ordering::Greater,
        Ordering::Equal => first.cmp(second),
    }
}

fn process(contents: &str) -> u32 {
    let mut hands_with_bids: Vec<(Vec<CardRank>, u32)> = vec![];
    for line in contents.lines() {
        if let Some((hand_str, bid_str)) = line.split_once(' ') {
            let hand: Vec<CardRank> = hand_str.chars().map(CardRank::new).collect();
            let bid: u32 = bid_str.parse::<u32>().unwrap();
            hands_with_bids.push((hand, bid));
        }
    }
    hands_with_bids.sort_by(|x, y| compare_hands(&x.0, &y.0));
    let mut sum: u32 = 0;
    for (i, hand_with_bid) in hands_with_bids.iter().enumerate() {
        sum += ((i + 1) as u32) * hand_with_bid.1;
    }
    sum
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    println!("result = {result}");
}
