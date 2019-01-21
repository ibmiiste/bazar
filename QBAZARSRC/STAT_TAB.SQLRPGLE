

       EXEC SQL
         SET OPTION COMMIT = *NONE;

       EXEC SQL
         CREATE TABLE Odrubigny.Table_Stat
           AS (SELECT *
                 FROM Qsys2.Systablestat)
           WITH
           DATA ;

       Return;

