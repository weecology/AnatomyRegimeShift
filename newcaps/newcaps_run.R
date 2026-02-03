#----------------------------------------------------------------
#
# Calculate New Individuals
#
#----------------------------------------------------------------

# ToDO: 
# 1) we have years with all counts but no new counts. Need to check 
#     that there really weren't new individuals (i.e. all recaps)
# 2) early years do not have *. Might need to create a new function 
#     that IDs 'newcaps' from the first appearance of a tag (with time 
#     buffer from last seen). This is probably already (or partially)
#     in the Supp code for cleaning tags. Using this would let us push 
#     back caps to 1977.
#
# 

annual_new = get_newcounts(controls_all)