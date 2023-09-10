# Function to determine the name of a poker hand
determine_poker_hand <- function(suits, ranks) {
  # Function to count the occurrences of each rank
  count_ranks <- function(ranks) {
    # Use the table to count the number of each rank
    rank_count <- table(ranks)
    return(rank_count)
  }
  
  # Function to check for a flush
  is_flush <- function(suits) {
    # The unique() function will give non-repeat elements and if length of unique suits equals 1, 
    # five cards are of the same suit as a  flush. Eg: "Hearts"，"Hearts"，"Hearts"，"Hearts"，"Hearts"
    return(length(unique(suits)) == 1)
  }
  
  # Function to check for a straight
  is_straight <- function(ranks) {
    # sort() function gives out a vector in ascending order, here we unique() the vector first
    sorted_ranks <- sort(unique(ranks))
    # if the length of sorted_ranks equals 5, it means all the five cards are unique.
    # Then we calculate the max&min difference, if it is 4, then it's a straight as five cards in a sequence. Eg: 3,4,5,6,7
    return(length(sorted_ranks) == 5 && max(sorted_ranks) - min(sorted_ranks) == 4)
  }
  # For each player's hand, count the number of rank, and then check if it is a flush or straight or both
  # Count the occurrences of each rank
  rank_count <- count_ranks(ranks)
  
  # Check for a flush
  flush <- is_flush(suits)
  
  # Check for a straight
  straight <- is_straight(ranks)
  
  # Determine the poker hand it should start from highest to lowest
  # A, K, Q, J, 10, all the same suit: is_flush() and is_straight() return TRUE and largest rank is "ACE"
  if (flush && straight && max(ranks) == 13) {
    return("Royal Flush")
    # Five cards in a sequence, all in the same suit: is_flush() and is_straight() return TRUE
  } else if (max(rank_count) == 4) {
    return("Four of a Kind")
    # All four cards of the same rank: max_count is 4
  } else if (max(rank_count) == 4) {
    return("Four of a Kind")
    # Three of a kind with a pair: max_count is 3 and their should be two unique rank(3,2)
  } else if (max(rank_count) == 3 && length(rank_count) == 2) {
    return("Full House")
    # Any five cards of the same suit, but not in a sequence: is_flush() returns TRUE
  } else if (flush) {
    return("Flush")
    # Five cards in a sequence, but not of the same suit: is_straight() returns TRUE
  } else if (straight) {
    return("Straight")
    # Three cards of the same rank: max_count is 3
  } else if (max(rank_count) == 3) {
    return("Three of a Kind")
    # Two different pairs: max_count is 2 and their should be three unique rank(2,2,1)
  } else if (max(rank_count) == 2 && length(rank_count) == 3) {
    return("Two Pair")
    # Two cards of the same rank: max_count is 2
  } else if (max(rank_count) == 2) {
    return("One Pair")
    # When you haven't made any of the hands above.
  } else {
    return("High Card")
  }
}

# Function to simulate dealing a round of poker
deal_poker_round <- function(num_players) {
  if (num_players > 10) {
    print("Error: Maximum number of players is 10.")
    return(NULL)
  }
  # Define the suits and ranks in a deck of cards
  # There should be 4 suits, generate 13 of each to combine with ranks 1-13
  suits <- rep(c("Hearts", "Diamonds", "Clubs", "Spades"), each = 13)
  # There should be 13 ranks, generate 4 times, each time will be combined with one suit
  ranks <- rep(c(1:13), times = 4)
  
  # Create a deck of cards, in each row, it will generate in order "Hearts 2"-"Heart 13"-"Diamonds 1"-"Diamonds 13"-"Clubs 1"-"Clubs 13"-"Spades     1"-"Spades 13", namely all the cards 
  deck <- data.frame(Suit = suits, Rank = ranks)
  
  # Shuffle the deck, sample() will give a random sequence of 52 rows
  deck <- deck[sample(nrow(deck)), ]
  
  # Deal cards to players
  # Create a list of players' hands
  hands <- list()
  # Start from one to the last player
  for (i in 1:num_players) {
    # From the (i-1) * 5+ 1 row to the i * 5 row， this loops in scale of 5 rows
    start <- (i - 1) * 5 + 1
    end <- i * 5
    # Take out these 5 rows which are 5 real cards
    hand <- deck[start:end, ]
    # Give these 5  cards to player i's hand    
    hands[[i]] <- hand
    # Print Player i's hand
    cat("Player", i, "hand:\n")
    # Print the 5 cards which have been taken out and given to player i
    print(hand)
    # Determine player i's poker hand with 5 cards' suit and rank
    cat("Hand Name:", determine_poker_hand(hand$Suit, hand$Rank), "\n\n")
  }
}