# coordination20
Welcome to our repository, presenting the property verification and testing examples from coordination 2020 paper,
Towards a Formally Verified EVM in Production Environment.

## This Repository

### safe_math
This directory includes 
1. safemath.mlw, which presents the specification of Safe Math library. After Why3 installation, run `why3 prove safemath.mlw`
will generate the verification conditions for the function definitions.
2. safemath directory, which is created automatically when running `why3 ide safemath.mlw`. This command will also start a session where you can attempt to prove goals that are generated from (.mlw) file. You can save the session after a proof attempt. The proof session state will be stored in an XML file named why3session.xml in the directory. 
### open_auction
This directory includes 
1. simple_auction.mlw, which presents the specification of Open Auction contract. The general idea of the simple auction contract is that everyone can send their bids when the auction is not over. If a higher bid is raised compared with the current recorded highestBid, the previously highest bidder will get their money back through withdrawl. After the end of the bidding, the beneficiary will claim the money.
2. simple_auction directory. You can check the why3 session information through `why3 session info [options] <session directory>` for various informations
about the session, e.g., proof statistics. 
