# This is a sample Julia/JuMP script to help you understand its syntax
# Parts of the script can help you complete Problem 2 of HW 8

# The script is based on the Dartboard 2.0 case and associated data files
# However, it solves for a different problem than the one you need to solve in HW8
# The problem we will solve is:
#   how to allocate counties to Dartboard's *** three existing DCs ***
#   during 2012-2013
#   in order to minimize transportation costs
# Note that we use a different planning horizon: *** January 2012 to December 2013 ***
# We base our analysis on actual sales (not predicted sales)

# Set working directory
# Make sure the data file <Dartboard_historical.csv> is in the working Directory

# In this script we will use the DataFrames and Geodesy packages
#   along with the JuMP and Gurobi packages
# If you haven't used any of these packages before, you need to add them:
# Pkg.add("DataFrames")
# Pkg.add("Geodesy")
# Pkg.add("JuMP")
# Pkg.add("Gurobi")
# Pkg.update()

using DataFrames
using Geodesy
using JuMP
using Gurobi  #You can instead use GLPK

##### Loading the historical data file as a dataframe
df = readtable("Dartboard_historical.csv")
# Selecting only the pertinent variables
df = df[:,[:FIPS_Code,:Latitude,:Longitude,:Week_Num,:Sales]]
# Loading DC data
dc = readtable("Dartboard_DCs.csv")
# Selecting only the three existing DCs
dc = dc[1:3,:]

##### Wrangling
# Extracting data for the period 2011 to 2012 only (i.e weeks 1 to 104).
# *** This is a different planning horizon from the one described in the case ***
df_1to104 = df[(df.Week_Num .<= 104),:]
# Extracting data for the last 8 weeks in 2013 - the "peak" demand
df_97to104 = df_1to104[(df_1to104.Week_Num .> 96),:]

# Summarzing but County (while keeping Latitude and Longitude info)
df_1to104_county = by(df_1to104, [:FIPS_Code, :Latitude, :Longitude], :Sales => sum)
df_97to104_county = by(df_97to104, [:FIPS_Code, :Latitude, :Longitude], :Sales => sum)

##### Defining parameters
num_dc = size(dc,1)
num_counties = size(df_1to104_county,1)

# We'll work everything in terms of pallets
# dividing by 1000 converts dollars to pallets
# (You can equivalently work in terms of dollar sales or SQF)

demand_1to104 = df_1to104_county[:,:Sales_sum] / 1000
demand_97to104 = df_97to104_county[:,:Sales_sum] / 1000

dc_cap_min = dc[:,:Current_Size] * 5 / 13.5
dc_cap_max = dc[:,:Max_Size] * 5 / 13.5

distances = zeros(num_dc, num_counties)
for i=1:num_dc, j=1:num_counties
    distances[i,j] = distance(LLA(dc[i,:Latitude], dc[i,:Longitude],0.0),
                              LLA(df_1to104_county[j,:Latitude], df_1to104_county[j,:Longitude],0.0))/1609.34 # meters per mile
end

distances

trans_cost = 1.55/20 # cost per pallet mile

##### Optimization Model

main_mod = Model(solver=GurobiSolver());

# The allocation decision of of DCs to counties
@variable(main_mod, x[1:num_dc,1:num_counties], Bin);

# Minimize the sum of transportation costs over the 2012-2013 period
@objective(main_mod, Min, trans_cost*sum(demand_1to104[c]*sum(distances[i,c]*x[i,c] for i=1:num_dc) for c=1:num_counties))

# Each county should use one and only one DC
@constraint(main_mod, nosplit[c=1:num_counties], sum(x[i,c] for i=1:num_dc) == 1)

# Keep peak 8-week period inventory below DC capacityC
@constraint(main_mod, dc_capacity[i=1:num_dc], sum(demand_97to104[c]*x[i,c] for c=1:num_counties) <= dc_cap_max[i])

show(main_mod)

solve(main_mod)

println("Objective value: ", getobjectivevalue(mymod))
# Objective value: $137,838,585

# Can also use getvalue(x) to inspect the allocation solution
