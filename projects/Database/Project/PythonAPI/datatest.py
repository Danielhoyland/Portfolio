from flask import Flask, jsonify
from flask_mysqldb import MySQL
from datetime import time
from decimal import Decimal



app = Flask(__name__)

app.config['MYSQL_HOST'] = 'localhost'
app.config['MYSQL_USER'] = 'root'
app.config['MYSQL_PASSWORD'] = ''
app.config['MYSQL_DB'] = 'timetableDB'


mysql = MySQL(app)


@app.route('/', methods=['GET'])
def defaultEndpoint():
    return jsonify("No functionality yet for default handler!")

#The database query tasks
@app.route('/1', methods=['GET'])
def endp1():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Course.ID, Lesson.Room, Room.Building FROM Course INNER JOIN Lesson ON Course.ID = Lesson.Course_ID INNER JOIN Room ON Lesson.Room = Room.Number WHERE Course.Lecturer IS NULL;')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/2', methods=['GET'])
def endp2():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT c.ID, c.Description, s.Season, s.Year FROM `Course` c JOIN Taught_by tb ON c.ID = tb.Course_ID JOIN Teacher t ON t.User_ID = tb.Teacher_ID JOIN Semester s ON c.Semester_ID = s.ID WHERE t.User_ID = 5 AND s.Season="Fall" AND s.Year = 2022')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/3', methods=['GET'])
def endp3():
    cur = mysql.connection.cursor()
    
    start_time = time(hour=4, minute=0, second=0)
    end_time = time(hour=18, minute=0, second=0)
    
    info = cur.execute('SELECT Lesson.Room, Course.ID, Course.Description, Lesson.Lesson_Date, Lesson.Start_time, Lesson.End_time FROM Course INNER JOIN Lesson ON Course.ID = Lesson.Course_ID WHERE Lesson.Room = 102 AND Lesson.Lesson_Date = "2023-02-13" AND Lesson.Start_time >= %s AND Lesson.End_time <= %s;', (start_time, end_time))
    
    if info > 0:
        info = cur.fetchall()
        # Convert the timedelta objects to strings
        info = [(room, course_id, desc, date, str(start), str(end)) for room, course_id, desc, date, start, end in info]
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/4', methods=['GET'])
def endp4():
    cur = mysql.connection.cursor()
    
    target_time = time(hour=15, minute=0, second=0)
    
    info = cur.execute('SELECT Course.ID, Course.Description, Lesson.Lesson_Date, Lesson.Start_time, Lesson.End_time FROM Course INNER JOIN Lesson ON Course.ID = Lesson.Course_ID WHERE Lesson.Room = 102 AND Lesson.Lesson_Date = "2023-02-13" AND Lesson.Start_time <= %s AND Lesson.End_time >= %s;', (target_time, target_time))
    
    if info > 0:
        info = cur.fetchall()
        # Convert the timedelta objects to strings
        info = [(course_id, desc, date, str(start), str(end)) for course_id, desc, date, start, end in info]
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/5', methods=['GET'])
def endp5():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Course.Description, Users.First_name, Users.Last_name, Users.Email FROM Course JOIN Taught_by ON Course.ID = Taught_by.Course_ID JOIN Teacher ON Taught_by.Teacher_ID = Teacher.User_ID JOIN Users ON Teacher.User_ID = Users.ID')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/6', methods=['GET'])
def endp6():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Course.Description, Course.Institute, Course.Faculty FROM Course JOIN Taught_by ON Course.ID = Taught_by.Course_ID JOIN Teacher ON Taught_by.Teacher_ID = Teacher.User_ID WHERE Teacher.User_ID = 5;')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404
    
@app.route('/7', methods=['GET'])
def endp7():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Course.Description, Room.Number AS Room_Number, Room.Building FROM Course JOIN Lesson ON Course.ID = Lesson.Course_ID JOIN Room ON Lesson.Room = Room.Number JOIN Users ON Course.Lecturer = Users.ID WHERE Users.First_name = "Emily" AND Users.Last_name = "Davis";')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404
    
@app.route('/8', methods=['GET'])
def endp8():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Room.Number, Room.Building FROM Room WHERE Room.Number NOT IN ( SELECT Booking.Room_Nr FROM Booking WHERE Booking.Start_time <= "15:00:00" AND Booking.End_time >= "14:00:00" AND Booking.Booking_Date = "2023-04-22");')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/9', methods=['GET'])
def endp9():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Booking.Description, Room.Number, Room.Building FROM Booking JOIN Room ON Booking.Room_Nr = Room.Number WHERE Booking.Person_ID = 3')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404
    
@app.route('/10', methods=['GET'])
def endp10():
    cur = mysql.connection.cursor()

    info = cur.execute('SELECT Room.Number, Room.Building, Booking.Description, Booking.Start_time, Booking.End_time, Users.First_name, Users.Last_name FROM Room LEFT JOIN Booking ON Room.Number = Booking.Room_Nr LEFT JOIN Users ON Booking.Person_ID = Users.ID ORDER BY Room.Number, Booking.Start_time;')

    if info > 0:
        info = cur.fetchall()
        # Convert the timedelta objects to strings
        info = [(room_num, building, desc, str(start), str(end), first_name, last_name) if start and end else (room_num, building, desc, start, end, first_name, last_name) for room_num, building, desc, start, end, first_name, last_name in info]
        cur.close()
        return jsonify(info), 200
    else:
        return jsonify("nothing found..."), 404
    
@app.route('/11', methods=['GET'])
def endp11():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Room.Number, Room.Type, COUNT(*) AS Reservations FROM Room LEFT JOIN Booking ON Room.Number = Booking.Room_Nr GROUP BY Room.Number, Room.Type ORDER BY Room.Number')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/12', methods=['GET'])
def endp12():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Teacher.User_ID, Users.First_name, Users.Last_name, Course.Semester_ID, COUNT(Taught_by.Course_ID) AS num_courses FROM Teacher INNER JOIN Users ON Teacher.User_ID = Users.ID INNER JOIN Taught_by ON Teacher.User_ID = Taught_by.Teacher_ID INNER JOIN Course ON Taught_by.Course_ID = Course.ID GROUP BY Teacher.User_ID, Course.Semester_ID ORDER BY num_courses DESC;')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/13', methods=['GET'])
def endp13():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Users.First_name, Users.Last_name, Course.Description, Room.Number, Room.Building FROM Taught_by JOIN Teacher ON Taught_by.Teacher_ID = Teacher.User_ID JOIN Users ON Teacher.User_ID = Users.ID JOIN Course ON Taught_by.Course_ID = Course.ID JOIN Lesson ON Lesson.Course_ID = Course.ID JOIN Room ON Room.Number = Lesson.Room ORDER BY Users.Last_name ASC, Users.First_name ASC')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/14', methods=['GET'])
def endp14():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Users.First_name, Users.Last_name, SUM(TIME_TO_SEC(TIMEDIFF(Lesson.End_time, Lesson.Start_time))/3600) AS Total_hours FROM Users JOIN Teacher ON Users.ID = Teacher.User_ID JOIN Taught_by ON Teacher.User_ID = Taught_by.Teacher_ID JOIN Course ON Taught_by.Course_ID = Course.ID JOIN Lesson ON Course.ID = Lesson.Course_ID GROUP BY Teacher.User_ID ORDER BY Total_hours DESC;')
    
    if info > 0:
        data = cur.fetchall()

        # Convert Decimal values to float
        data = [[float(val) if isinstance(val, Decimal) else val for val in row] for row in data]

        cur.close()
        return jsonify(data),200
    else:
        return jsonify("nothing found..."),404

@app.route('/15', methods=['GET'])
def endp15():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Course.Description, Users.First_name, Users.Last_name, Users.Email FROM Course  JOIN Lesson ON Course.ID = Lesson.Course_ID JOIN Taught_by ON Course.ID = Taught_by.Course_ID JOIN Teacher ON Taught_by.Teacher_ID = Teacher.User_ID JOIN Users ON Teacher.User_ID = Users.ID WHERE DAYOFWEEK(Lesson.Lesson_Date) = 2')
    
    if info > 0:
        info = cur.fetchall()
        cur.close()
        return jsonify(info),200
    else:
        return jsonify("nothing found..."),404

@app.route('/16', methods=['GET'])
def endp16():
    cur = mysql.connection.cursor()
    info = cur.execute('SELECT Users.First_name, Users.Last_name, AVG(Course.nr_students) AS avg_students FROM Teacher JOIN Taught_by ON Teacher.User_ID = Taught_by.Teacher_ID JOIN Users ON Teacher.User_ID = Users.ID JOIN Course ON Taught_by.Course_ID = Course.ID GROUP BY Teacher.User_ID ORDER BY avg_students DESC')
    
    if info > 0:
        data = cur.fetchall()

        # Convert Decimal values to float
        data = [[float(val) if isinstance(val, Decimal) else val for val in row] for row in data]

        cur.close()
        return jsonify(data),200
    else:
        return jsonify("nothing found..."),404

#starting server/app
if __name__ == '__main__':
    app.run()