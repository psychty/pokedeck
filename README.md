# Tyler Pokedeck

Christmas 2023, I think it is time for Pokemon to enter the chat.

My six year old, J, got the training pokemon set for christmas and we played it about eight times in 48 hours and my nostalgia induced dopamine levels hit the roof; I'm pretty sure J loved it too.

Fast forward to new years day, and the opportunity to sift through some random pokemon cards I bought from ebay for this very life stage flooded my dopamine receptors once again, only to be pushed even higher by my brain asking if I could use R to help catalogue and progress ~my~ our new collection.

I am hoping to use R to read in the collection, and some open source database of pokemon to tell us what cards are missing (we have some stage 1 and 2 pokemon that require some basic cards to evolve from) and what cards we could benefit from getting (we have some basic cards that no doubt can be evolved).

I may also look at the types of pokemon we have, such as fire or water, and whether we need to try to increase the amount of particular types to build specific decks.

~~//TODO call my parents and find out if I have any old pokemon cards stored in the loft of our family home.~~

Boo, the RT collection of cards is no more

# ~~TCGPlayer app~~

~~I used the TCGPlayer mobile app to scan the existing cards and export them to csv.~~

# Good old manual data entry

I tried using TCGPlayer app but scanning each card individually ended up taking longer than i thought, drained the phone battery, and getting the precise angle and distance away to capture each card was not consistent across the whole collection. In the end, we felt like arranging the cards by type, and then grouping them by name and evolution was the simplest way to process our cards. 

# PokemonTCG.io

This is a free api for all cards, it does not require an api key but has rate limits so we may find we hit those at some point when we start looping through our raw_card_data

Third-party application rate limits depend on your API key. By default, requests are limited to 20,000/day.

If you are not use an API key, the rate limit is 1,000 requests per day, and a maxium of 30 per minute.

We can query all sorts of parameters but most simply our query is looking for the name, we dont need to make it query the specific card ID or set ID as we're just looking to add the cards we need, not to build sets as collectors (aint nobody got time(money) for that).

# Next steps

What are we trying to achieve? 

I think we ultimately want to know what cards we need, and what cards we have an abundance of.

Maybe we could put this into a lightweight website hosted somewhere like netlify that we could access on the fly when we come across some cards in the wild.
