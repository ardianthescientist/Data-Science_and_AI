from flask import Flask, request
import pandas as pd
import sqlite3

# Flask build
app = Flask(__name__) 

# Database
def make_connection():
    connection = sqlite3.connect('austin_bikeshare.db')
    return connection

# Homepage
@app.route('/')
def route_home():
    return 'Hello World'


# All stations
@app.route('/stations/')
def route_all_stations():
    return get_all_stations().to_json()
    
def get_all_stations():
    conn = make_connection()
    query = f"""SELECT * FROM stations"""
    return pd.read_sql_query(query, conn)

# Station by id
@app.route('/stations/<station_id>')
def route_station_by_id(station_id):
    return get_station_by_id(station_id).to_json()
    
def get_station_by_id(station_id):
    conn = make_connection()
    query = f"""SELECT * FROM stations WHERE stations.station_id == {station_id}"""
    return pd.read_sql_query(query, conn)

# Add station
@app.route('/stations/add', methods=['POST']) 
def route_add_station():
    data = pd.Series(eval(request.get_json(force=True)))
    data = tuple(data.fillna('').values)

    return insert_into_stations(data)

def insert_into_stations(data):
    conn = make_connection()
    query = f"""INSERT INTO stations values {data}"""
    try:
        conn.execute(query)
    except:
        return 'Error'
    conn.commit()
    return 'OK'


# All trips
@app.route('/trips/')
def route_all_trips():
    return get_all_trips().to_json()

def get_all_trips():
    conn = make_connection()
    query = f"""SELECT * FROM trips"""
    return pd.read_sql_query(query, conn)

# Trip by id
@app.route('/trips/<trip_id>')
def route_trip_by_id(trip_id):
    return get_trip_by_id(trip_id).to_json()

def get_trip_by_id(trip_id):
    conn = make_connection()
    query = f"""SELECT * FROM trips WHERE trips.id = {trip_id}"""
    return pd.read_sql_query(query, conn)

# Add trip
@app.route('/trips/add', methods=['POST']) 
def route_add_trip():
    data = pd.Series(eval(request.get_json(force=True)))
    data = tuple(data.fillna('').values)

    return insert_into_trips(data)

def insert_into_trips(data):
    conn = make_connection()
    query = f"""INSERT INTO trips VALUES {data}"""
    try:
        conn.execute(query)
    except:
        return "Error"
    conn.commit()
    return "Ok"


# Average duration
@app.route('/trips/average_duration')
def route_trip_avg_duration():
    return get_trip_avg_duration().to_json()

def get_trip_avg_duration():
    conn = make_connection()
    query = '''
            SELECT
                AVG(duration_minutes) AS average_duration
            FROM
                trips
            '''
    return pd.read_sql_query(query, conn)

# Average duration by id
@app.route('/trips/average_duration/<bike_id>')
def route_trip_avg_duration_by_id(bike_id):
    return get_trip_avg_duration_by_id(bike_id).to_json()

def get_trip_avg_duration_by_id(bike_id):
    conn = make_connection()
    query = f'''
            SELECT
                AVG(duration_minutes) AS average_duration
            FROM
                trips
            WHERE
                bikeid = {bike_id}
            '''
    return pd.read_sql_query(query, conn)


# Bike activity of each station by period
@app.route('/station-bike-activity_by_period', methods=['POST'])
def route_station_bike_activity_by_period():
    input_data = request.get_json()
    specified_date = input_data['period']

    return get_station_bike_activity_by_period(specified_date).to_json()

def get_station_bike_activity_by_period(specified_date):
    conn = make_connection()
    query = f"""
            SELECT *
            FROM
                trips
            WHERE
                start_time LIKE '{specified_date}%'
            """
    selected_data = pd.read_sql_query(query, conn)

    result = selected_data.groupby('start_station_id').agg({
        'bikeid' : 'count', 
        'duration_minutes' : 'mean'
    })

    return result

# Contoh input JSON
@app.route('/json', methods=['POST'])
def json_example():
    
    req = request.get_json(force=True)
    
    name = req['name']
    age = req['age']
    address = req['address']
    
    return (f'''Hello {name}, your age is {age}, and your address in {address}''')

# App runner
if __name__ == '__main__':
    app.run(debug=True, port=5000)