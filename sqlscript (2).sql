create table Provinces( 
    Province_Code varchar2(16) primary key, 
    Province_Name varchar2(50) 
);

create table Municipalities( 
    Mun_Code varchar2(20) primary key, 
    Name Varchar2(60), 
    Avg_Population number, 
    Province_Code varchar2(16), 
    foreign key (Province_Code) references Provinces(Province_Code) 
);

create table Facilities( 
    Facility_ID varchar2(15) primary key, 
    F_name varchar2(60), 
    Capacity number, 
    Address Varchar2(60), 
    Mun_Code varchar2(20), 
     
    foreign key (Mun_Code) references Municipalities(Mun_Code) 
);

create table Room( 
    Room_Number varchar2(60) primary key, 
    Facility_ID varchar2(15) NOT NULL, 
    Description varchar2(60) 
);

create table Activity( 
    Act_References varchar2(20) primary key, 
    Act_Name varchar2(100) , 
    Catergory varchar2(50) check (Catergory IN ('Cinema', 'Circus', 'Dance', 'Music', 'Bullfights', 'Theatre', 'Exhibition', 'Workshop')) 
);

create table Uses( 
    Room_Number varchar2(60) , 
    Act_References varchar2(20), 
    use_DATE TIMESTAMP, 
    foreign key (Room_Number) references Room(Room_Number), 
    foreign key (Act_References) references Activity(Act_References) 
);

create sequence province_sequence 
start with 1 
increment by 1 
nocache;

create sequence municipality_seq 
start with 1 
increment by 1 
nocache;

create sequence facility_seq 
start with 1 
increment by 1 
nocache;

create sequence room_seq 
start with 1 
increment by 1 
nocache;

create sequence activity_seq 
start with 1 
increment by 1 
nocache;

insert into Provinces (Province_Code, Province_Name) values ('PROV' || province_sequence.NEXTVAL, 'GAUTENG');

insert into Provinces (Province_Code, Province_Name) values ('PROV' || province_sequence.NEXTVAL, 'WESTERN CAPE');

insert into Municipalities (Mun_Code, Name, Avg_Population, Province_Code) values ('MUN' || municipality_seq.NEXTVAL, 'City of Tshwane', 3300000, 'PROV1');

insert into Municipalities (Mun_Code, Name, Avg_Population, Province_Code) values ('MUN' || municipality_seq.NEXTVAL, 'City of CAPE TOWN', 5300000, 'PROV2');

INSERT INTO Facilities (Facility_ID, F_name, Capacity, Address, Mun_Code) VALUES ('FAC' || facility_seq.NEXTVAL, 'State Theatre', 1000, 'Pretoria CBD-GP', 'MUN1');

INSERT INTO Facilities (Facility_ID, F_name, Capacity, Address, Mun_Code) VALUES ('FAC' || facility_seq.NEXTVAL, 'Artscape Theatre', 900, 'Cape Town CBD-CP', 'MUN2');

insert into Room (Room_Number, Facility_ID, Description) values ('ROOM' || room_seq.NEXTVAL, 'FAC1', 'Main Auditorium');

insert into Room (Room_Number, Facility_ID, Description) values ('ROOM' || room_seq.NEXTVAL, 'FAC2', 'Workshop Hall');

INSERT INTO Activity (Act_References, Act_Name, Catergory) VALUES ('ACT' || activity_seq.NEXTVAL, 'Swan Lake Ballet', 'Dance');

INSERT INTO Activity (Act_References, Act_Name, Catergory) VALUES ('ACT' || activity_seq.NEXTVAL, 'Cape Town Jazz Festival', 'Music');

INSERT INTO Uses (Room_Number, Act_References, use_DATE) VALUES ('ROOM1', 'ACT1', TO_TIMESTAMP('2023-07-01 20:00:00', 'YYYY-MM-DD HH24:MI:SS'));

INSERT INTO Uses (Room_Number, Act_References, use_DATE) VALUES ('ROOM2', 'ACT2', TO_TIMESTAMP('2026-07-02 12:30:00', 'YYYY-MM-DD HH24:MI:SS'));

SELECT m.Mun_Code, m.Name AS Municipality_Name, p.Province_Name 
FROM Municipalities m 
JOIN Provinces p ON m.Province_Code = p.Province_Code 
WHERE NOT EXISTS ( 
    SELECT 1 
    FROM Facilities f 
    JOIN Room r ON f.Facility_ID = r.Facility_ID 
    JOIN Uses u ON r.Room_Number = u.Room_Number 
    JOIN Activity a ON u.Act_References = a.Act_References 
    WHERE f.Mun_Code = m.Mun_Code 
    AND a.Catergory = 'Music' 
) 
ORDER BY p.Province_Name, m.Name;

SELECT Province_Name 
FROM Provinces 
WHERE Province_Code IN ( 
    SELECT Province_Code 
    FROM Municipalities 
    WHERE Avg_Population >= 4000000 
);

CREATE OR REPLACE PROCEDURE Province_Capacity_Utilization_procedure 
IS 
BEGIN 
    -- Report Header 
    DBMS_OUTPUT.PUT_LINE('PROVINCE FACILITY UTILIZATION REPORT'); 
    DBMS_OUTPUT.PUT_LINE('---------------------------------------------'); 
    DBMS_OUTPUT.PUT_LINE( 
        RPAD('Province Name', 20) || ' ' || 
        RPAD('Facilities', 12) || ' ' || 
        RPAD('Total Capacity', 16) || ' ' || 
        RPAD('Activities', 12) || ' ' || 
        'Utilization %' 
    ); 
    DBMS_OUTPUT.PUT_LINE( 
        RPAD('-', 20, '-') || ' ' || 
        RPAD('-', 12, '-') || ' ' || 
        RPAD('-', 16, '-') || ' ' || 
        RPAD('-', 12, '-') || ' ' || 
        RPAD('-', 14, '-') 
    ); 
 
    -- Main logic 
    FOR rec IN ( 
        SELECT  
            p.Province_Name, 
            COUNT(DISTINCT f.Facility_ID) AS facility_count, 
            NVL(SUM(f.Capacity), 0) AS total_capacity, 
            COUNT(u.Act_References) AS activity_count, 
            ROUND( 
                CASE  
                    WHEN NVL(SUM(f.Capacity), 0) > 0 THEN  
                        (COUNT(u.Act_References) / SUM(f.Capacity)) * 100  
                    ELSE 0  
                END, 2 
            ) AS utilization_percentage 
        FROM  
            Provinces p 
            JOIN Municipalities m ON p.Province_Code = m.Province_Code 
            JOIN Facilities f ON m.Mun_Code = f.Mun_Code 
            LEFT JOIN Room r ON f.Facility_ID = r.Facility_ID 
            LEFT JOIN Uses u ON r.Room_Number = u.Room_Number 
        GROUP BY  
            p.Province_Name 
        ORDER BY  
            utilization_percentage DESC 
    ) LOOP 
        DBMS_OUTPUT.PUT_LINE( 
            RPAD(rec.Province_Name, 20) || ' ' || 
            RPAD(TO_CHAR(rec.facility_count, '9,999'), 12) || ' ' || 
            RPAD(TO_CHAR(rec.total_capacity, '9,999,999'), 16) || ' ' || 
            RPAD(TO_CHAR(rec.activity_count, '9,999'), 12) || ' ' || 
            TO_CHAR(rec.utilization_percentage, '990.99') || '%' 
        ); 
    END LOOP; 
     
    -- Footer 
    DBMS_OUTPUT.PUT_LINE('---------------------------------------------'); 
    DBMS_OUTPUT.PUT_LINE('* Utilization % = (Total Activities / Total Capacity) × 100'); 
EXCEPTION 
    WHEN OTHERS THEN 
        DBMS_OUTPUT.PUT_LINE('Error generating report: ' || SQLERRM); 
END; 
/

BEGIN 
    Province_Capacity_Utilization_procedure; 
END; 
/

