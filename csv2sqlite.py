import csv
import sqlite3
import sys


class Csv2SqliteGenerator(object):
    """
    Convert a csv to a sqlite table
    """

    def __init__(self, database_name, table_name, file_name, delimiter=','):
       	file_handler = open(file_name)
        self.csv_data = csv.reader(file_handler, delimiter=delimiter)
        self.conn = sqlite3.connect(database_name)
        self.cursor = self.conn.cursor()
        self.table_name = table_name
        self._drop_table_if_exists()
        for row in self.csv_data:
            if self.csv_data.line_num == 1:
                self._do_create_table(row)
                continue
            query = "INSERT INTO %s VALUES (%s)" % (self.table_name, ','.join(['?' for x in row]))
            self.cursor.execute(query, row)
        self.conn.commit()
        self.conn.close()


    def _drop_table_if_exists(self):
        try:
            self.cursor.execute("DROP TABLE %s" % self.table_name)
        except Exception:
            pass			
        
    
    def _do_create_table(self, row):
        createstatement = "CREATE TABLE %s" % self.table_name
        query = '%s (%s)' % (createstatement, ','.join(['"%s" text' % field for field in row]))
        self.cursor.execute(query)




if __name__ == '__main__':

	# The command should receive all the arguments
	if len(sys.argv) != 5:
		print("Mode de usar: python csvtosqlite.py databasename table_name filename delimiter")
		exit(1)

	# Set all variables in parameters
	args = sys.argv[1:]
	database_name = args[0]
	table_name = args[1]
	file_name = args[2]
	delimiter = args[3]

	# Print the variables that will be used
	print("Creating Sqlite table with the following parameters:")
	print("Database name->%s" % database_name)
	print("Table name->%s" % table_name)
	print("File name->%s" % file_name)
	print("Delimiter->%s" % delimiter)
	
	table = Csv2SqliteGenerator(database_name, table_name, file_name, delimiter)
	
	print("ok")
	
