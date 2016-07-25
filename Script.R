# 
# # 
# # Gwilym/Jon/Gavin (cc. Mark/Mirjam)
# # 
# # I'd be keen to plan a timetable for this work, and agreeing how we will try to use the data, particularly to 
# look at the question of suburbanisation of poverty. I don't think it is a huge amount of effort and it would be 
# good to have it set up so the full code is available through GitHub so others are able to follow and comment on 
# the process. We'd also try to make the resulting data available through UBDC. Have copied in Mark and Mirjam since 
# they may be interested in contributing or commenting on method. Steps would be as follows:
# # 
# # June/July
# # 1. Get the existing SIMD data for DZ-2001 from 2004 - should have this from the pollution paper work but need to 
#   make sure that, for each year, we have: population, SIMD score and rank, and Income Deprivation %.
# # 
# # 2. Using the old DZ boundaries, run the decentralisation code on the Income Deprived for the four cities for each year, 
# and check whether we see similar pattern to the suburbanisation paper. This also acts as a baseline against which we 
# can check the re-aggregated data.
# # 
# # 3. Agree a procedure for making our 'best' estimate of overall SIMD score/rank, and Income Deprivation, on new DZ-2011 
# boundaries. We can do better than simply looking at the nearest old DZ for each new DZ centroid by using information about 
# variations within each DZ, utilising Census data at OA level. We have: deprivation scores for OAs in 2001 (Mirjam Allik's 
# paper); and a lookup file showing how the population of each OA-2001 is apportioned to each DZ-2011 (Paul Norman's file). 
# The approach would be:
# a. use OA-2001 deprivation decile to apportion DZ-2001 deprivation to the OA-2001;
# b. use Norman's file to re-aggregate to DZ-2011.
# 
# 4. Re-run the decentralisation code using the estimates of Income Deprivation for the new DZ boundaries and compare with 
# 2. To do this, we need to be able to identify the four cities in terms of the DZ-2011 boundaries.
# 
# Early August
# 5. Finally, on the day that the SIMD2016 are released, run the decentralisation code on that data and also produce simple 
# maps showing absolute changes. Put out a short summary.
# 
# Does this sound like a plan?
# 
# Regards
# 
# Nick
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 