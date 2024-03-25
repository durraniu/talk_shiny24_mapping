import redis
import time
import pandas as pd

#------------------------------------------------------
# Read vehicle coordinate data (example)
# Data was actually generated from a driving simulator
# that was captured via the sockets module
# ED means Ego vehicle Driver
#------------------------------------------------------
veh_coords = pd.read_csv("data/veh_coordinate_data.csv")


#------------------------------------------------------
# Connect with redis database. This database was 
# created via Ubuntu on Windows. The name of this 
# database is minisim. 
# Make sure the redis server is already started:
## sudo service redis-server 
# When you're done, do:
## redis-cli flushdb 
#------------------------------------------------------
r = redis.Redis(host="127.0.0.1", port=6379, db=0)


#------------------------------------------------------
# This loop will store the data in redis database.
# Each 0.01 seconds, a new x,y pair is generated 
# from the veh_coords datafarme
#------------------------------------------------------
for i in range(len(veh_coords)):
    time.sleep(0.015)
    print(veh_coords.iloc[[i]])
    r.rpush('position', veh_coords.iloc[[i]].to_json(orient='records'))
    